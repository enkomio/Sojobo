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
        match processContainer.Memory.GetMemoryRegion(instruction.Address) with
        | Some region ->
            if instruction.IsCall() then
                let instruction = instruction :?> IntelInstruction
                match instruction.Info.Operands with
                | OneOperand op ->
                    match op with
                    | OprMem (_, _, disp, _) when disp.IsSome ->
                        let procAddr = 
                            if processContainer.PointerSize = 32
                            then processContainer.Memory.ReadMemory<UInt32>(uint64 disp.Value) |> uint64
                            else 
                                let effectiveValue = 
                                    processContainer.ProgramCounter.As<UInt64>() + 
                                    uint64 instruction.Length +
                                    uint64 disp.Value
                                processContainer.Memory.ReadMemory<UInt64>(effectiveValue)

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

            let disassembledInstruction = BinHandler.DisasmInstr region.Handler false true instruction 
            let instructionBytes = BinHandler.ReadBytes(region.Handler , instruction.Address, int32 instruction.Length)                
            let hexBytes = BitConverter.ToString(instructionBytes).Replace("-"," ")
            String.Format("0x{0,-10} {1, -30} {2} {3}", instruction.Address.ToString("X") + ":", hexBytes, disassembledInstruction, functionName)
        | None ->
            String.Empty
        
    let disassembleCurrentInstructionIR(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        BinHandler.LiftInstr handler instruction
        |> Array.map(fun stmt ->
            String.Format("type: {0,-10} => {1}", stmt.GetType().Name, LowUIR.Pp.stmtToString(stmt))
        )    

    let private mapSectionsAtAddress(baseAddress: UInt64, handler: BinHandler, memoryManager: MemoryManager) =
        Helpers.getPe(handler)
        |> Option.iter(fun pe ->    
            handler.FileInfo.GetSections()
            |> Seq.map(fun section ->
                let sectionHeader = 
                    pe.SectionHeaders 
                    |> Seq.find(fun sc -> sc.Name.Equals(section.Name, StringComparison.OrdinalIgnoreCase))

                // copy the section content                        
                let sectionBuffer = Array.zeroCreate<Byte>(int32 section.Size)
                if sectionHeader.SizeOfRawData > 0 then
                    let fileSectionContent = handler.ReadBytes(section.Address, sectionHeader.SizeOfRawData)
                    let bytesToCopy = min sectionHeader.SizeOfRawData (int32 section.Size)
                    Array.Copy(fileSectionContent, sectionBuffer, bytesToCopy)
            
                let sectionBaseAddress = baseAddress + uint64 sectionHeader.VirtualAddress
                let sectionHandler = 
                    BinHandler.Init(
                        handler.ISA, 
                        ArchOperationMode.NoMode, 
                        false, 
                        sectionBaseAddress, 
                        sectionBuffer
                    )
                (section, sectionBuffer, sectionHandler, sectionBaseAddress, Helpers.getSectionPermission(sectionHeader))
            ) 
            |> Seq.map(fun (section, buffer, sectionHandler, sectionBaseAddress, permission) -> {
                BaseAddress = sectionBaseAddress
                Content = buffer
                Handler = sectionHandler
                Permission = permission
                Type = String.Empty
                Info = String.Format("{0} - {1}", section.Name, handler.FileInfo.FilePath |> Path.GetFileName)
            })
            |> Seq.iter(memoryManager.AddLibraryMemoryRegion)

            // create memory holes if necessary
            // TODO: round region size to page size in order to avoid this gaps
            handler.FileInfo.GetSections()
            |> Seq.sortBy(fun s -> s.Address)
            |> Seq.pairwise
            |> Seq.iter(fun (bottomSec, topSec) ->
                let holeStart = bottomSec.Address + bottomSec.Size
                let holeSize = topSec.Address - holeStart
                if holeSize > 0UL then
                    memoryManager.AllocateMemory(holeStart, int32 holeSize, Permission.Readable, regionType = "<Reserved>")
            )
        )

    let private mapPeHeaderAtAddress(baseAddress: UInt64, handler: BinHandler, memoryManager: MemoryManager) =
        Helpers.getPe(handler)
        |> Option.iter(fun pe ->
            let fileInfo = handler.FileInfo
            let struct (buffer, _) = fileInfo.BinReader.ReadBytes(int32 pe.PEHeaders.PEHeader.SizeOfHeaders, 0)
        
            {
                BaseAddress = baseAddress
                Content = buffer
                Handler =
                    BinHandler.Init(
                        handler.ISA,
                        ArchOperationMode.NoMode, 
                        false, 
                        baseAddress,
                        buffer
                    )
                Permission = Permission.Readable
                Type = String.Empty
                Info = String.Format("PE - {0}", fileInfo.FilePath |> Path.GetFileName)
            }
            |> memoryManager.AddLibraryMemoryRegion

            // create memory hole if necessary, recompute the offset due to possible relocation
            let firstSection = handler.FileInfo.GetSections() |> Seq.minBy(fun s -> s.Address)
            let firstSectionRva = firstSection.Address - uint64 pe.PEHeaders.PEHeader.ImageBase
            let firstSectionStart = baseAddress + firstSectionRva

            let holeStart = baseAddress + uint64 pe.PEHeaders.PEHeader.SizeOfHeaders
            let holeSize = firstSectionStart - holeStart
            memoryManager.AllocateMemory(holeStart, int32 holeSize, Permission.Readable)
        )
        
    let mapPeAtAddress(handler: BinHandler, memoryManager: MemoryManager, baseAddress: UInt64) =
        mapPeHeaderAtAddress(baseAddress, handler, memoryManager)
        mapSectionsAtAddress(baseAddress, handler, memoryManager)

    let mapPe(handler: BinHandler, memoryManager: MemoryManager) =
        Helpers.getPe(handler)
        |> Option.iter(fun pe ->
            mapPeAtAddress(handler, memoryManager, pe.PEHeaders.PEHeader.ImageBase)
        )