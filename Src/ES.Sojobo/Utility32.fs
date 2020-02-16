namespace ES.Sojobo

open System
open System.IO
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR
open B2R2.BinFile
open B2R2.FrontEnd.Intel

module Utility32 = 
    let disassemble(processContainer: IProcessContainer, instruction: Instruction) =
        let mutable functionName = String.Empty
        let handler = processContainer.Memory.GetMemoryRegion(instruction.Address).Handler

        if instruction.IsCall() then
            let instruction = instruction :?> IntelInstruction
            match instruction.Info.Operands with
            | OneOperand op ->
                match op with
                | OprMem (_, _, disp, _) when disp.IsSome ->
                    let procAddr = 
                        if processContainer.GetPointerSize() = 32
                        then processContainer.Memory.ReadMemory<UInt32>(uint64 disp.Value) |> uint64
                        else processContainer.Memory.ReadMemory<UInt64>(uint64 disp.Value)

                    match processContainer.TryGetSymbol(procAddr) with
                    | Some symbol -> functionName <- String.Format("; <&{0}> [{1}]", symbol.Name, symbol.LibraryName)
                    | None -> ()
                | OprReg reg ->
                    let register = processContainer.Cpu.GetRegister(reg.ToString())
                    match processContainer.TryGetSymbol(register.Value |> BitVector.toUInt64) with
                    | Some symbol -> functionName <- String.Format("; <&{0}> [{1}]", symbol.Name, symbol.LibraryName)
                    | None -> ()
                | _ -> ()
            | _ -> ()

        let disassembledInstruction = BinHandler.DisasmInstr handler false true instruction 
        let instructionBytes = BinHandler.ReadBytes(handler , instruction.Address, int32 instruction.Length)                
        let hexBytes = BitConverter.ToString(instructionBytes).Replace("-"," ")
        String.Format("0x{0,-10} {1, -30} {2} {3}", instruction.Address.ToString("X") + ":", hexBytes, disassembledInstruction, functionName)
        
    let disassembleCurrentInstructionIR(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        BinHandler.LiftInstr handler instruction
        |> Array.map(fun stmt ->
            String.Format("type: {0,-10} => {1}", stmt.GetType().Name, LowUIR.Pp.stmtToString(stmt))
        )    

    let private mapSectionsAtAddress(baseAddress: UInt64, handler: BinHandler, memoryManager: MemoryManager) =
        let pe = Helpers.getPe(handler)        
        handler.FileInfo.GetSections()
        |> Seq.map(fun section ->
            let sectionHeader = 
                pe.SectionHeaders 
                |> Seq.find(fun sc -> sc.Name.Equals(section.Name, StringComparison.OrdinalIgnoreCase))

            // copy the section content                        
            let sectionBuffer = Array.zeroCreate<Byte>(int32 section.Size)
            let fileSectionContent = handler.ReadBytes(section.Address, sectionHeader.SizeOfRawData)
            let bytesToCopy = min sectionHeader.SizeOfRawData (int32 section.Size)
            Array.Copy(fileSectionContent, sectionBuffer, bytesToCopy)
            
            let sectionBaseAddress = baseAddress + uint64 sectionHeader.VirtualAddress
            let sectionHandler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, false, sectionBaseAddress, sectionBuffer)
            (section, sectionBuffer, sectionHandler, sectionBaseAddress, Helpers.getSectionPermission(sectionHeader))
        ) 
        |> Seq.map(fun (section, buffer, sectionHandler, sectionBaseAddress, permission) -> {
            BaseAddress = sectionBaseAddress
            Content = buffer
            Handler = sectionHandler
            Permission = permission
            Type = handler.FileInfo.FilePath |> Path.GetFileName
            Info = section.Name
        })
        |> Seq.iter(memoryManager.AddLibraryMemoryRegion)

        // create memory holes if necessary
        handler.FileInfo.GetSections()
        |> Seq.sortBy(fun s -> s.Address)
        |> Seq.pairwise
        |> Seq.iter(fun (bottomSec, topSec) ->
            let holeStart = bottomSec.Address + bottomSec.Size
            let holeSize = topSec.Address - holeStart
            memoryManager.AllocateMemory(holeStart, int32 holeSize, Permission.Readable, regionInfo = "<Reserved>")
        )

    let private mapPeHeaderAtAddress(baseAddress: UInt64, handler: BinHandler, memoryManager: MemoryManager) =
        let pe = Helpers.getPe(handler)
        let fileInfo = handler.FileInfo
        let struct (buffer, _) = fileInfo.BinReader.ReadBytes(int32 pe.PEHeaders.PEHeader.SizeOfHeaders, 0)
        
        {
            BaseAddress = baseAddress
            Content = buffer
            Handler =
                BinHandler.Init(
                    ISA.OfString "x86", 
                    ArchOperationMode.NoMode, 
                    false, 
                    baseAddress,
                    buffer
                )
            Permission = Permission.Readable
            Type = String.Empty
            Info = fileInfo.FilePath |> Path.GetFileName
        }
        |> memoryManager.AddLibraryMemoryRegion

        // create memory hole if necessary, recompute the offset due to possible relocation
        let firstSection = handler.FileInfo.GetSections() |> Seq.minBy(fun s -> s.Address)
        let firstSectionRva = firstSection.Address - uint64 pe.PEHeaders.PEHeader.ImageBase
        let firstSectionStart = baseAddress + firstSectionRva

        let holeStart = baseAddress + uint64 pe.PEHeaders.PEHeader.SizeOfHeaders
        let holeSize = firstSectionStart - holeStart
        memoryManager.AllocateMemory(holeStart, int32 holeSize, Permission.Readable)
        
    let mapPeAtAddress(handler: BinHandler, memoryManager: MemoryManager, baseAddress: UInt64) =
        mapPeHeaderAtAddress(baseAddress, handler, memoryManager)
        mapSectionsAtAddress(baseAddress, handler, memoryManager)

    let mapPe(handler: BinHandler, memoryManager: MemoryManager) =
        let pe = Helpers.getPe(handler)
        mapPeAtAddress(handler, memoryManager, pe.PEHeaders.PEHeader.ImageBase)