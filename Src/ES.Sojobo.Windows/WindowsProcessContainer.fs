namespace ES.Sojobo.Windows

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel

type WindowsProcessContainer(pointerSize: Int32) as this =  
    inherit BaseProcessContainer(pointerSize)
    let _handles = new List<Handle>()    
    let _iat = new List<Symbol>() 
    let mutable _lastInstruction: Instruction option = None   

    let setEntryPoint(handler: BinHandler) =
        this.UpdateActiveMemoryRegion(this.Memory.GetMemoryRegion(handler.FileInfo.EntryPoint.Value))

        let instructionPointer =
            if pointerSize = 32 then        
                createVariableWithValue(string Register.EIP, EmulatedType.DoubleWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint.Value 32<rt>)
            else
                createVariableWithValue(string Register.RIP, EmulatedType.QuadWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint.Value 64<rt>)

        this.Cpu.SetRegister(instructionPointer)

    let setupStackRegisters() =
        // set base address
        let baseStackAddress = this.Memory.Stack.BaseAddress + uint64(this.Memory.Stack.Content.Length - 0x10)
        let baseStackPointer = 
            if pointerSize = 32 then  
                createVariableWithValue(string Register.EBP, EmulatedType.DoubleWord, BitVector.ofUInt64 baseStackAddress 32<rt>)
            else
                createVariableWithValue(string Register.RBP, EmulatedType.QuadWord, BitVector.ofUInt64 baseStackAddress 64<rt>)
        this.Cpu.SetRegister(baseStackPointer)

        // set top address
        let topStackAddress = this.Memory.Stack.BaseAddress + uint64(baseStackAddress - 0x100UL)
        let topStackPointer = 
            if pointerSize = 32 then               
                createVariableWithValue(string Register.ESP, EmulatedType.DoubleWord, BitVector.ofUInt64 topStackAddress 32<rt>)
            else
                createVariableWithValue(string Register.RSP, EmulatedType.QuadWord, BitVector.ofUInt64 topStackAddress 64<rt>)
        this.Cpu.SetRegister(topStackPointer)

        
        
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

    member internal this.Initialize(handler: BinHandler) =
        Utility32.mapPe(handler, this.Memory)
        setupStackRegisters()
        setEntryPoint(handler)
        resolveIATSymbols(handler)

    member this.Initialize(buffer: Byte array) =
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        this.Initialize(handler)

    member this.Initialize(filename: String) =  
        this.SetFileName(filename)
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, filename)        
        this.Initialize(handler)

    default this.GetImportedFunctions() =
        Seq.readonly _iat

    default this.Handles = _handles |> Seq.toArray   
    
    member this.AddHandle(handle: Handle) =
        _handles.Add(handle)

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
        with get() = 
            if this.PointerSize = 32 then this.Cpu.GetRegister(Register.EIP.ToString())
            else this.Cpu.GetRegister(Register.RIP.ToString())

    default this.GetCallStack() = [|
        let mutable baseStack = 
            (if this.PointerSize = 32 then this.Cpu.GetRegister(Register.EBP.ToString())
            else this.Cpu.GetRegister(Register.RBP.ToString())).Value
            |> BitVector.toUInt64

        let increment = this.PointerSize / 8 |> uint64
        let readMemory(addr: UInt64) =
            if this.PointerSize = 32 then (this.Memory.ReadMemory<UInt32>(addr) |> uint64)
            else this.Memory.ReadMemory<UInt64>(addr)

        let mutable retValue = readMemory(baseStack + increment)
        while retValue <> 0UL do
            yield retValue
            baseStack <- readMemory(baseStack) |> uint64
            retValue <- readMemory(baseStack + increment)
    |]