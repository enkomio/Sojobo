namespace ES.Sojobo

open System
open System.Collections.Generic
open System.Reflection
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2.BinFile.PE
open Win32
open System.Reflection.PortableExecutable

type Win32ProcessContainer() as this =  
    inherit BaseProcessContainer()

    let _memoryManager = new MemoryManager()
    let _iat = new List<Symbol>()
    let _stepEvent = new Event<IProcessContainer>()
    let mutable _activeRegion: MemoryRegion option = None    
    
    let setEntryPoint(handler: BinHandler) =
        _activeRegion <- 
            _memoryManager.GetMemoryRegion(handler.FileInfo.EntryPoint)
            |> Some

        // save the EIP registry value
        let eip = string Register.EIP
        let eipValue = createVariableWithValue(eip, EmulatedType.DoubleWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint 32<rt>)
        this.Variables.Add(eip, eipValue)

    let getPe(handler: BinHandler) =
        let fileInfo = handler.FileInfo
        fileInfo.GetType().GetField("pe", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(fileInfo) :?> PE        
        
    let mapPeHeader(handler: BinHandler, pe: PE) =
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
        |> _memoryManager.AddMemoryRegion

    let getSectionProtection(sectionHeader: SectionHeader) =
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

    let mapSections(handler: BinHandler, pe: PE) =
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
        |> Seq.iter(_memoryManager.AddMemoryRegion)

    let setupRegisters() =
        [
            // segments
            createVariableWithValue(string Register.SS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.SSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ES, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ESBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 teb32Address 32<rt>)
            createVariableWithValue(string Register.GS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.GSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)

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
                0x19D000UL, 
                0x3000, 
                MemoryProtection.Read ||| MemoryProtection.Write
            ) with 
                Type = "Stack"
                Info = handler.FileInfo.FilePath
        }
        _memoryManager.AddMemoryRegion(stackRegion)

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
        let stack = 
            _memoryManager.GetMemoryMap()
            |> Seq.find(fun memRegion -> memRegion.Type.Equals("Stack", StringComparison.OrdinalIgnoreCase))
        
        // add teb
        let teb = 
            {Activator.CreateInstance<TEB32>() with
                StackBase = uint32 stack.BaseAddress + uint32 stack.Content.Length
                StackLimit = uint32 stack.BaseAddress
                Self = teb32Address
                ProcessEnvironmentBlock = peb32Address
            }
        let tebMemoryRegion = createMemoryRegion(uint64 teb32Address, 0x1000, MemoryProtection.Read)
        Utility.writeStructure(teb, 0, tebMemoryRegion.Content)
        _memoryManager.AddMemoryRegion(tebMemoryRegion)     

        // add peb
        let peb = Activator.CreateInstance<PEB32>()
        let pebMemoryRegion = createMemoryRegion(uint64 peb32Address, 0x1000, MemoryProtection.Read)        
        Utility.writeStructure(peb, 0, pebMemoryRegion.Content)
        _memoryManager.AddMemoryRegion(pebMemoryRegion)            

    let initialize(handler: BinHandler) =
        let pe = getPe(handler)
        mapPeHeader(handler, pe)
        mapSections(handler, pe)
        addStackRegion(handler)
        setEntryPoint(handler)
        resolveIATSymbols(handler)
        setupRegisters()    
        createStructures()

    default this.Step = _stepEvent.Publish   
    default this.Memory = _memoryManager

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

    default this.GetActiveMemoryRegion() =
        _activeRegion.Value
        
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

    default this.GetCallStack() = [|
        let mutable ebp = this.GetVariable("EBP").Value |> BitVector.toUInt32
        let mutable retValue = BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
        while retValue <> 0ul do
            yield uint64 retValue
            ebp <- BitConverter.ToUInt32(this.Memory.ReadMemory(uint64 ebp, 4) , 0)
            retValue <- BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
    |]

    default this.GetPointerSize() =
        32