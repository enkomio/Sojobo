namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open Win32

type Win32ProcessContainer() as this =  
    inherit BaseProcessContainer()

    let _va = new Dictionary<UInt64, MemoryRegion>()
    let _iat = new List<Symbol>()
    let _stepEvent = new Event<IProcessContainer>()
    let mutable _activeRegion: MemoryRegion option = None    
    
    let addRegion(memRegion: MemoryRegion) =        
        _va.[memRegion.BaseAddress] <- memRegion

    let getMemoryRegion(programCounter: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            programCounter >= startAddr && programCounter <= endAddr
        )

    let writeMemory(address: UInt64, value: Byte array) =
        // copy the memory
        let region = getMemoryRegion(address)
        let offset = region.Handler.FileInfo.TranslateAddress address
        Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)

    let setEntryPoint(handler: BinHandler) =
        _activeRegion <- 
            getMemoryRegion(handler.FileInfo.EntryPoint)
            |> Some

        // save the EIP registry value
        let eip = string Register.EIP
        let eipValue = createVariableWithValue(eip, EmulatedType.DoubleWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint 32<rt>)
        this.Variables.Add(eip, eipValue)

    let mapSections(handler: BinHandler) =            
        handler.FileInfo.GetSections()
        |> Seq.map(fun section -> {
            BaseAddress = section.Address
            Content = handler.FileInfo.BinReader.Bytes
            Handler = handler
            Protection = section.Kind
            Type = section.Name
            Info = handler.FileInfo.FilePath
        })
        |> Seq.iter(addRegion)

    let setupRegisters() =
        [
            // segments
            createVariableWithValue(string Register.SS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.SSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.CS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.CSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.DS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.DSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.ES, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.ESBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.GS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.GSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)

            // general purpose registers
            createVariableWithValue(string Register.EAX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EBX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ECX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ESI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)

            // flag registers
            createVariableWithValue(string Register.OF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.DF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.IF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.TF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.SF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.ZF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.AF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.PF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.CF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
        ] |> List.iter(fun register -> this.Variables.Add(register.Name, register))

    let addStackRegion(handler: BinHandler) =
        let stackRegion = {
            createMemoryRegion(
                0x1000UL, 
                0x1000, 
                SectionKind.WritableSection ||| SectionKind.ExecutableSection
            ) with 
                Type = "Stack"
                Info = handler.FileInfo.FilePath
        }
        addRegion(stackRegion)

        // set ESP value
        let esp = string Register.ESP
        let startAddress = int32 stackRegion.BaseAddress + int32 stackRegion.Content.Length - 8
        let espValue = createVariableWithValue(esp, EmulatedType.DoubleWord, BitVector.ofInt32 startAddress 32<rt>)
        this.Variables.Add(esp, espValue)

        // set EBP value equals to ESP
        let ebp = string Register.EBP
        let ebpValue = createVariableWithValue(ebp, EmulatedType.DoubleWord, espValue.Value)
        this.Variables.Add(ebp, ebpValue)

    let resolveIATSymbols(handler: BinHandler) =
        handler.FileInfo.GetSymbols()
        |> Seq.iter(fun symbol ->
            if not(String.IsNullOrEmpty(symbol.LibraryName)) && (symbol.Kind = SymbolKind.ExternFunctionType || symbol.Kind = SymbolKind.FunctionType) then 
                _iat.Add(symbol)
        )

    let createStructures() =
        let peb = createMemoryRegion(uint64 peb32Address, 0x1000, SectionKind.WritableSection ||| SectionKind.ExtraSection)        
        addRegion(peb)
        
        let tib = createMemoryRegion(uint64 teb32Address, 0x1000, SectionKind.WritableSection ||| SectionKind.ExtraSection)
        let teb32Struct = {Activator.CreateInstance<TEB32>() with ProcessEnvironmentBlock = peb32Address}        
        Utility.writeStructure(teb32Struct, 0, tib.Content)
        addRegion(tib)        

    let initialize(handler: BinHandler) =
        mapSections(handler)
        addStackRegion(handler)
        setEntryPoint(handler)
        resolveIATSymbols(handler)
        setupRegisters()    
        createStructures()

    default this.Step = _stepEvent.Publish    

    default this.GetVariable(name: String) =
        this.Variables.[name]

    default this.SetVariable(value: EmulatedValue) =
        if value.IsTemp
        then this.TempVariables.[value.Name] <- value
        else this.Variables.[value.Name] <- value

    member this.Initialize(buffer: Byte array) =
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        initialize(handler)

    member this.Initialize(filename: String) =  
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)        
        initialize(handler)

    default this.GetImportedFunctions() =
        _iat |> Seq.readonly

    default this.GetMemoryRegion(address: UInt64) =
        getMemoryRegion(address)

    default this.GetActiveMemoryRegion() =
        _activeRegion.Value

    default this.UpdateMemoryRegion(oldRegion: MemoryRegion, newRegion: MemoryRegion) =
        _va.[oldRegion.BaseAddress] <- newRegion

    default this.WriteMemory(address: UInt64, value: Byte array) =
        writeMemory(address, value)

    default this.GetInstruction() =
        let programCounter = this.GetProgramCounter()
        BinHandler.ParseInstr (this.GetActiveMemoryRegion().Handler) (programCounter.Value |> BitVector.toUInt64)

    member this.ReadNextInstruction() =      
        _stepEvent.Trigger(this)
        let instruction = this.GetInstruction()
        let programCounter = this.GetProgramCounter()
        this.Variables.[programCounter.Name] <- 
            {programCounter with
                Value = BitVector.add programCounter.Value (BitVector.ofUInt32 instruction.Length 32<rt>)
            }
        instruction

    default this.GetProgramCounter() =
        this.Variables.["EIP"]
    
    default this.GetProgramCounterValue() =
        this.GetProgramCounter().Value |> BitVector.toUInt64    

    default this.ReadMemory(address: UInt64, size: Int32) =
        let memRegion = this.GetMemoryRegion(address)
        let offset = address - memRegion.BaseAddress |> int32
        let buffer = Array.zeroCreate<Byte>(size)
        Array.Copy(memRegion.Content, offset, buffer, 0, size)
        buffer

    default this.GetArgument(position: Int32) =
        let ebp = this.GetVariable("EBP", EmulatedType.DoubleWord).Value |> BitVector.toUInt32
        let address = ebp + uint32 (position + 2) * 4ul
        let buffer = this.ReadMemory(uint64 address, sizeof<UInt32>)
        let varName = this.GetTempName(string position, EmulatedType.DoubleWord)        
        {createVariable(varName, EmulatedType.DoubleWord) with Value = BitVector.ofArr(buffer)}

    default this.GetCallStack() = [|
        let mutable ebp = this.GetVariable("EBP").Value |> BitVector.toUInt32
        let mutable retValue = BitConverter.ToUInt32(this.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
        while retValue <> 0ul do
            yield uint64 retValue
            ebp <- BitConverter.ToUInt32(this.ReadMemory(uint64 ebp, 4) , 0)
            retValue <- BitConverter.ToUInt32(this.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
    |]

    default this.GetPointerSize() =
        32