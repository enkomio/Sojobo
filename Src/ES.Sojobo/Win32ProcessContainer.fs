namespace ES.Sojobo

open System
open System.Collections.Generic
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR
open B2R2.BinFile
open B2R2.BinIR.LowUIR
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel

type Win32ProcessContainer() =    
    let _va = new Dictionary<UInt64, MemoryRegion>()    
    let _tempVariables = new Dictionary<String, EmulatedValue>()
    let _variables = new Dictionary<String, EmulatedValue>()    
    let mutable _programCounter = 0UL
    let mutable _activeRegion: MemoryRegion option = None
    
    let addRegion(memRegion: MemoryRegion) =        
        _va.[memRegion.BaseAddress] <- memRegion

    let getMemoryRegion(programCounter: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Size
            programCounter >= startAddr && programCounter <= endAddr
        )

    let setEntryPoint(handler: BinHandler) =
        _programCounter <- handler.FileInfo.EntryPoint
        _activeRegion <- 
            getMemoryRegion(_programCounter)
            |> Some

        // save the EIP registry value
        let eipIndex = int32 Register.EIP
        let eipValue = createVariableWithValue(string eipIndex, EmulatedType.DoubleWord, BitVector.ofUInt64 _programCounter 32<rt>)
        _variables.Add(string eipIndex, eipValue)

    let mapSections(handler: BinHandler) =
        handler.FileInfo.GetSections()
        |> Seq.map(fun section -> {
            BaseAddress = section.Address
            Size = int64 section.Size
            Handler = handler
            Protection = section.Kind
            Type = String.Empty
            Info = handler.FileInfo.FilePath
        })
        |> Seq.iter(addRegion)

    let setupRegisters() =
        [
            createVariableWithValue(string (int32 Register.EAX), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
            createVariableWithValue(string (int32 Register.EBX), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
            createVariableWithValue(string (int32 Register.ECX), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
            createVariableWithValue(string (int32 Register.EDX), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
            createVariableWithValue(string (int32 Register.ESI), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
            createVariableWithValue(string (int32 Register.EDI), EmulatedType.DoubleWord, BitVector.ofInt32 0 32<rt>)
        ] |> List.iter(fun register ->
            _variables.Add(register.Name, register)
        )

    let addStackRegion(handler: BinHandler) =
        // must create a new handler for each newly created region
        let stackBuffer = Array.zeroCreate<Byte>(8192)
        let isa = ISA.OfString "x86"
        let stackHandler = BinHandler.Init(isa, ArchOperationMode.NoMode, FileFormat.RawBinary, Addr.MinValue, stackBuffer)

        let stack = {
            BaseAddress = 0x1000UL
            Size = 0x1000L
            Handler = stackHandler
            Protection = SectionKind.WritableSection ||| SectionKind.ExecutableSection
            Type = "Stack"
            Info = handler.FileInfo.FilePath
        }
        addRegion(stack)

        // set ESP value
        let espIndex = int32 Register.ESP
        let startAddress = int32 stack.BaseAddress + int32 (stack.Size / 2L)
        let espValue = createVariableWithValue(string espIndex, EmulatedType.DoubleWord, BitVector.ofInt32 startAddress 32<rt>)
        _variables.Add(string espIndex, espValue)

        // set EBP value equals to ESP
        let ebpIndex = int32 Register.EBP
        let ebpValue = createVariableWithValue(string ebpIndex, EmulatedType.DoubleWord, espValue.Value)
        _variables.Add(string ebpIndex, ebpValue)

    let initialize(handler: BinHandler) =
        mapSections(handler)
        addStackRegion(handler)
        setEntryPoint(handler)
        setupRegisters()

    let getTempName(index: Int32, emuType: EmulatedType) =
        let size =  Utility.getTypeSize(emuType)
        String.Format("T_{0}:{1}", index, size)

    member this.GetOrCreateTemporaryVariable(index: Int32, emuType: EmulatedType) =
        let name = getTempName(index, emuType)
        match _tempVariables.TryGetValue(name) with
        | (true, value) -> value
        | _ -> 
            let variable = {createVariable(name, emuType) with IsTemp = true}
            _tempVariables.[name] <- variable
            variable
            
    member this.GetVariable(index: Int32, emuType: EmulatedType) =        
        match _variables.TryGetValue(string index) with
        | (true, value) -> value
        | _ ->
            let name = getTempName(index, emuType)
            _tempVariables.[name]

    member this.SetVariable(value: EmulatedValue) =
        if value.IsTemp
        then _tempVariables.[value.Name] <- value
        else _variables.[value.Name] <- value

    member this.ClearTemporaryVariables() =
        _tempVariables.Clear()

    member this.Initialize(buffer: Byte array) =
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, FileFormat.RawBinary, Addr.MinValue, buffer)
        initialize(handler)

    member this.Initialize(filename: String) =  
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, FileFormat.PEBinary, Addr.MinValue, filename)
        initialize(handler)

    member this.GetMemoryRegion(address: UInt64) =
        getMemoryRegion(address)

    member this.GetActiveMemoryRegion() =
        _activeRegion.Value

    member this.UpdateMemoryRegion(oldRegion: MemoryRegion, newRegion: MemoryRegion) =
        _va.[oldRegion.BaseAddress] <- newRegion

    member this.GetInstruction() =
        let instruction = BinHandler.ParseInstr (this.GetActiveMemoryRegion().Handler) _programCounter
        _programCounter <- _programCounter + uint64 instruction.Length
        instruction