namespace ES.Sojobo

open System
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR
open B2R2.BinFile
open B2R2.FrontEnd.Intel

module Utility = 
    let formatCurrentInstruction(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        let mutable functionName = String.Empty

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
        
    let formatCurrentInstructionIR(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        BinHandler.LiftInstr handler instruction
        |> Array.map(fun stmt ->
            String.Format("type: {0,-10} => {1}", stmt.GetType().Name, LowUIR.Pp.stmtToString(stmt))
        )

    let internal mapPeHeader(handler: BinHandler, memoryManager: MemoryManager) =
        let pe = Helpers.getPe(handler)
        let fileInfo = handler.FileInfo
        let struct (buffer, _) = fileInfo.BinReader.ReadBytes(int32 pe.PEHeaders.PEHeader.SizeOfHeaders, 0)
        
        {
            BaseAddress = pe.PEHeaders.PEHeader.ImageBase
            Content = buffer
            Handler =
                BinHandler.Init(
                    ISA.OfString "x86", 
                    ArchOperationMode.NoMode, 
                    false, 
                    pe.PEHeaders.PEHeader.ImageBase, 
                    buffer
                )
            Permission = Permission.Readable
            Type = fileInfo.FilePath
            Info = fileInfo.FilePath
        }
        |> memoryManager.AddMemoryRegion

    let internal mapSections(handler: BinHandler, memoryManager: MemoryManager) =
        let pe = Helpers.getPe(handler)
        handler.FileInfo.GetSections()
        |> Seq.map(fun section ->
            let sectionHeader = 
                pe.SectionHeaders 
                |> Seq.find(fun sc -> sc.Name.Equals(section.Name, StringComparison.OrdinalIgnoreCase))
            
            let sectionSize = min sectionHeader.SizeOfRawData (int32 section.Size)            
            let buffer = Array.zeroCreate<Byte>(max sectionHeader.SizeOfRawData (int32 section.Size))
            Array.Copy(handler.ReadBytes(section.Address, sectionSize), buffer, sectionSize)
                        
            let sectionHandler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, false, section.Address, buffer)
            (section, buffer, sectionHandler, Helpers.getSectionPermission(sectionHeader))
        ) 
        |> Seq.map(fun (section, buffer, sectionHandler, permission) -> {
            BaseAddress = section.Address
            Content = buffer
            Handler = sectionHandler
            Permission = permission
            Type = section.Name
            Info = handler.FileInfo.FilePath
        })
        |> Seq.iter(memoryManager.AddMemoryRegion)