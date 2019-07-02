namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

[<AbstractClass>]
type BaseProcessContainer(pointerSize: Int32) =
    let mutable _activeRegion: MemoryRegion option = None
    let _beforeEmulationEvent = new Event<IProcessContainer>()   
    let _afterEmulationEvent = new Event<IProcessContainer>()
    let _symbols = new Dictionary<UInt64, Symbol>()
    let _pid = Guid.NewGuid().GetHashCode() |> uint32

    member val PointerSize = pointerSize with get

    abstract ProgramCounter: EmulatedValue with get
    abstract GetImportedFunctions: unit -> Symbol seq
    abstract GetInstruction: unit -> Instruction    
    abstract GetInstruction: address:UInt64 -> Instruction  
    abstract GetCallStack: unit -> UInt64 array
    abstract Memory: MemoryManager with get
    abstract Cpu: Cpu with get

    member internal this.UpdateActiveMemoryRegion(memRegion: MemoryRegion) =
        _activeRegion <- Some memRegion

    member this.GetActiveMemoryRegion() =
        _activeRegion.Value

    member this.GetPointerSize() =
        pointerSize

    member internal this.SignalBeforeEmulation() =
        _beforeEmulationEvent.Trigger(this)

    member internal this.SignalAfterEmulation() =
        _afterEmulationEvent.Trigger(this)

    member this.BeforeEmulation = _beforeEmulationEvent.Publish 
    member this.AfterEmulation = _afterEmulationEvent.Publish 
    member this.Pid = _pid    

    member this.TryGetSymbol(address: UInt64) =
        match _symbols.TryGetValue(address) with
        | (true, symbol) -> Some symbol
        | _ -> None

    member this.SetSymbol(symbol: Symbol) =
        _symbols.[symbol.Address] <- symbol

    member internal this.ResetState() =
        this.Memory.Clear()
    
    interface IProcessContainer with
        member this.ProgramCounter
            with get() = this.ProgramCounter

        member this.GetPointerSize() =
            this.GetPointerSize()

        member this.GetImportedFunctions() =
            this.GetImportedFunctions()

        member this.GetInstruction() =
            this.GetInstruction()

        member this.GetInstruction(address: UInt64) =
            this.GetInstruction(address)

        member this.GetCallStack() =
            this.GetCallStack()
        
        member this.GetActiveMemoryRegion() =
            this.GetActiveMemoryRegion()

        member this.TryGetSymbol(address: UInt64) =
            this.TryGetSymbol(address)

        member this.SetSymbol(symbol: Symbol) =
            this.SetSymbol(symbol)

        [<CLIEvent>]
        member this.BeforeEmulation
            with get() = this.BeforeEmulation

        [<CLIEvent>]
        member this.AfterEmulation
            with get() = this.AfterEmulation

        member this.Memory
            with get() = this.Memory

        member this.Cpu
            with get() = this.Cpu

        member this.Pid
            with get() = this.Pid