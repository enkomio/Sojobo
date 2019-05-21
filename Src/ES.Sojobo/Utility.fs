namespace ES.Sojobo

open System
open System.Reflection
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR
open B2R2.BinFile.PE
open System.Reflection.PortableExecutable

module Utility =        
    let toArray(bitVector: BitVector) =
        let size = int32 <| BitVector.getType bitVector
        let value = BitVector.getValue bitVector
        match size with        
        | 8 -> [|byte value|]
        | 16 -> BitConverter.GetBytes(uint16 value)
        | 32 -> BitConverter.GetBytes(uint32 value)
        | 64 -> BitConverter.GetBytes(uint64 value)
        | _ -> failwith("Unexpected size: " + string size)

    let getType(regType: RegType) =
        match (RegType.toBitWidth regType) with
        | 1 -> EmulatedType.Bit
        | 8 -> EmulatedType.Byte
        | 16 -> EmulatedType.Word
        | 32 -> EmulatedType.DoubleWord
        | 64 -> EmulatedType.QuadWord
        | _ -> failwith("Invalid reg type size: " + regType.ToString())

    let getSize(emuType: EmulatedType) =
        match emuType with
        | EmulatedType.Bit -> 1
        | EmulatedType.Byte -> 8
        | EmulatedType.Word -> 16
        | EmulatedType.DoubleWord -> 32
        | EmulatedType.QuadWord -> 64

    let getTypeSize =
        getType >> getSize

    let formatCurrentInstruction(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        let disassembledInstruction = BinHandler.DisasmInstr handler false true instruction 
        let instructionBytes = BinHandler.ReadBytes(handler , instruction.Address, int32 instruction.Length)                
        let hexBytes = BitConverter.ToString(instructionBytes).Replace("-"," ")
        String.Format("0x{0,-10} {1, -30} {2}", instruction.Address.ToString("X") + ":", hexBytes, disassembledInstruction)
        
    let formatCurrentInstructionIR(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        BinHandler.LiftInstr handler instruction
        |> Array.map(LowUIR.Pp.stmtToString)

    let getTempName(index: String, emuType: EmulatedType) =
        let size =  getSize(emuType)
        String.Format("T_{0}:{1}", index, size)   

    let getPe(handler: BinHandler) =
        let fileInfo = handler.FileInfo
        fileInfo.GetType().GetField("pe", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(fileInfo) :?> PE

    let private getSectionProtection(sectionHeader: SectionHeader) =
        let characteristics = sectionHeader.SectionCharacteristics
        let mutable protection: MemoryProtection option = None
        
        if characteristics.HasFlag(SectionCharacteristics.MemRead) then 
            protection <- Some MemoryProtection.Read

        if characteristics.HasFlag(SectionCharacteristics.MemWrite) then 
            protection <-
                match protection with
                | Some p -> p ||| MemoryProtection.Write
                | None -> MemoryProtection.Write
                |> Some

        if characteristics.HasFlag(SectionCharacteristics.MemExecute) then 
            protection <-
                match protection with
                | Some p -> p ||| MemoryProtection.Execute
                | None -> MemoryProtection.Execute
                |> Some

        Option.defaultValue MemoryProtection.Read protection

    let mapPeHeader(handler: BinHandler, memoryManager: MemoryManager) =
        let pe = getPe(handler)
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
            Protection = MemoryProtection.Read
            Type = fileInfo.FilePath
            Info = fileInfo.FilePath
        }
        |> memoryManager.AddMemoryRegion

    

    let mapSections(handler: BinHandler, memoryManager: MemoryManager) =
        let pe = getPe(handler)
        handler.FileInfo.GetSections()
        |> Seq.map(fun section ->
            let sectionHeader = 
                pe.SectionHeaders 
                |> Seq.find(fun sc -> sc.Name.Equals(section.Name, StringComparison.OrdinalIgnoreCase))
            
            let sectionSize = min sectionHeader.SizeOfRawData (int32 section.Size)            
            let buffer = Array.zeroCreate<Byte>(max sectionHeader.SizeOfRawData (int32 section.Size))
            Array.Copy(handler.ReadBytes(section.Address, sectionSize), buffer, sectionSize)
                        
            let sectionHandler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, false, section.Address, buffer)
            (section, buffer, sectionHandler, getSectionProtection(sectionHeader))
        ) 
        |> Seq.map(fun (section, buffer, sectionHandler, protection) -> {
            BaseAddress = section.Address
            Content = buffer
            Handler = sectionHandler
            Protection = protection
            Type = section.Name
            Info = handler.FileInfo.FilePath
        })
        |> Seq.iter(memoryManager.AddMemoryRegion)