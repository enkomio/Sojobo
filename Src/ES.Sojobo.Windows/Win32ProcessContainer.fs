namespace ES.Sojobo.Windows

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel

type Win32ProcessContainer() as this =  
    inherit BaseProcessContainer(32)

    let _handles = new List<Handle>()
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
        Utility32.mapPeHeader(handler, _memoryManager)
        Utility32.mapSections(handler, _memoryManager)
        setupStackRegisters()
        setEntryPoint(handler)
        resolveIATSymbols(handler)
    
    default this.Memory = _memoryManager
    default this.Cpu = _cpu
    default this.Handles = _handles |> Seq.toArray

    member this.AddHandle(handle: Handle) =
        _handles.Add(handle)
        
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

    default this.GetInstruction(address: UInt64) =
        match _lastInstruction with
        | Some instruction when address = instruction.Address -> instruction
        | _ ->
            try
                _lastInstruction <- BinHandler.ParseInstr (this.GetActiveMemoryRegion().Handler) address |> Some
                _lastInstruction.Value
            with 
                :? IndexOutOfRangeException ->
                    // maybe the address is in another region, try to resolve the address
                    let memRegion = this.Memory.GetMemoryRegion(address)
                    BinHandler.ParseInstr memRegion.Handler address
        
    default this.GetInstruction() =
        this.GetInstruction(this.ProgramCounter.Value |> BitVector.toUInt64)

    default this.ProgramCounter
        with get() = this.Cpu.GetRegister("EIP")

    default this.GetCallStack() = [|
        let mutable ebp = this.Cpu.GetRegister("EBP").Value |> BitVector.toUInt64
        let increment = this.GetPointerSize() / 8 |> uint64

        let mutable retValue = this.Memory.ReadMemory<UInt32>(ebp + increment)
        while retValue <> 0ul do
            yield uint64 retValue
            ebp <- this.Memory.ReadMemory<UInt32>(ebp) |> uint64
            retValue <- this.Memory.ReadMemory<UInt32>(ebp + increment)
    |]