namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel

type Win32ProcessContainer() as this =  
    inherit BaseProcessContainer(32)

    let _memoryManager = new MemoryManager(32)
    let _iat = new List<Symbol>()
    let _cpu = new Cpu()
    let mutable _lastInstruction: Instruction option = None
    
    let setEntryPoint(handler: BinHandler) =
        this.UpdateActiveMemoryRegion(_memoryManager.GetMemoryRegion(handler.FileInfo.EntryPoint))

        // save the EIP registry value
        let eipValue = createVariableWithValue(string Register.EIP, EmulatedType.DoubleWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint 32<rt>)
        this.Cpu.SetRegister(eipValue)

    let setupStackRegisters() =
        // set ESP value
        let startAddress = int32 _memoryManager.Stack.BaseAddress + int32 _memoryManager.Stack.Content.Length - 8
        let espValue = createVariableWithValue(string Register.ESP, EmulatedType.DoubleWord, BitVector.ofInt32 startAddress 32<rt>)
        this.Cpu.SetRegister(espValue)

        // set EBP value equals to ESP
        let ebpValue = createVariableWithValue(string Register.EBP, EmulatedType.DoubleWord, espValue.Value)
        this.Cpu.SetRegister(ebpValue)
        
    let resolveIATSymbols(handler: BinHandler) =
        handler.FileInfo.GetSymbols()
        |> Seq.iter(fun symbol ->
            if 
                not(String.IsNullOrEmpty(symbol.LibraryName)) && 
                (symbol.Kind = SymbolKind.ExternFunctionType || symbol.Kind = SymbolKind.FunctionType) 
            then 
                this.SetSymbol(symbol)
                _iat.Add(symbol)
        )

    let initialize(handler: BinHandler) =
        Utility.mapPeHeader(handler, _memoryManager)
        Utility.mapSections(handler, _memoryManager)
        setupStackRegisters()
        setEntryPoint(handler)
        resolveIATSymbols(handler)
    
    default this.Memory = _memoryManager
    default this.Cpu = _cpu
        
    member this.Initialize(buffer: Byte array) =
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        initialize(handler)

    member this.Initialize(filename: String) =  
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)        
        initialize(handler)

    default this.GetImportedFunctions() =
        Seq.readonly _iat
        
    default this.GetInstruction() =
        let programCounter = this.ProgramCounter.Value |> BitVector.toUInt64
        match _lastInstruction with
        | Some instruction when programCounter = instruction.Address -> instruction
        | _ ->
            _lastInstruction <- BinHandler.ParseInstr (this.GetActiveMemoryRegion().Handler) (programCounter) |> Some
            _lastInstruction.Value

    default this.ProgramCounter
        with get() = this.Cpu.GetRegister("EIP")

    default this.GetCallStack() = [|
        let mutable ebp = this.Cpu.GetRegister("EBP").Value |> BitVector.toUInt32
        let mutable retValue = BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
        while retValue <> 0ul do
            yield uint64 retValue
            ebp <- BitConverter.ToUInt32(this.Memory.ReadMemory(uint64 ebp, 4) , 0)
            retValue <- BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
    |]