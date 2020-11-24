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
    let mutable _fileName = String.Empty
    let _memoryManager = new MemoryManager(pointerSize)
    let _cpu = new Cpu32()
    let _symbols = new Dictionary<UInt64, Symbol>()
    let _pid = 
        let rnd = new Random()
        rnd.Next(200, 4000) |> uint32
        
    abstract ProgramCounter: EmulatedValue with get
    abstract GetImportedFunctions: unit -> Symbol seq
    abstract GetInstruction: unit -> Instruction    
    abstract GetInstruction: address:UInt64 -> Instruction  
    abstract GetCallStack: unit -> UInt64 array        
    abstract Handles: Handle array with get

    abstract FileName: String with get
    default this.FileName 
        with get() = _fileName

    abstract PointerSize: Int32 with get
    default this.PointerSize 
        with get() = pointerSize

    abstract Memory: MemoryManager with get
    default this.Memory 
        with get() = _memoryManager

    abstract Cpu: Cpu with get
    default this.Cpu 
        with get() = upcast _cpu

    member this.SetFileName(fileName: String) =
        _fileName <- fileName

    member this.UpdateActiveMemoryRegion(memRegion: MemoryRegion) =
        _activeRegion <- Some memRegion

    member this.GetActiveMemoryRegion() =
        _activeRegion.Value
            
    member val Pid = _pid with get, set

    member this.TryGetSymbol(address: UInt64) =
        match _symbols.TryGetValue(address) with
        | (true, symbol) -> Some symbol
        | _ -> None

    member this.SetSymbol(symbol: Symbol) =
        _symbols.[symbol.Address] <- symbol

    member this.ResetState() =
        this.Memory.UnmapModules()
    
    interface IProcessContainer with
        member this.ProgramCounter
            with get() = this.ProgramCounter            
            
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

        member this.Memory
            with get() = this.Memory

        member this.Cpu
            with get() = this.Cpu

        member this.Handles
            with get() = this.Handles

        member this.FileName
            with get() = this.FileName

        member this.PointerSize
            with get() = this.PointerSize

        member this.Pid
            with get() = this.Pid
            and set(v) = this.Pid <- v