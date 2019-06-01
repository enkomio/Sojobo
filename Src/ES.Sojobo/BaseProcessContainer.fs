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
    let _stepEvent = new Event<IProcessContainer>()       

    member val internal Variables = new Dictionary<String, EmulatedValue>() with get
    member val internal TempVariables = new Dictionary<String, EmulatedValue>() with get
    member val PointerSize = pointerSize with get

    abstract ProgramCounter: EmulatedValue with get
    abstract SetRegister: EmulatedValue -> unit
    abstract GetRegister: name: String -> EmulatedValue    
    abstract GetImportedFunctions: unit -> Symbol seq
    abstract GetInstruction: unit -> Instruction    
    abstract GetCallStack: unit -> UInt64 array
    abstract Memory: MemoryManager with get
    abstract Cpu: Cpu with get

    member internal this.UpdateActiveMemoryRegion(memRegion: MemoryRegion) =
        _activeRegion <- Some memRegion

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = Helpers.getTempName(index, emuType)
        match this.TempVariables.TryGetValue(name) with
        | (true, value) -> value
        | _ -> 
            let variable = {createVariable(name, emuType) with IsTemp = true}
            this.TempVariables.[name] <- variable
            variable    

    member internal this.GetVariable(name: String, emuType: EmulatedType) =        
        match this.Variables.TryGetValue(name) with
        | (true, value) -> value
        | _ ->
            let name = Helpers.getTempName(name, emuType)
            this.TempVariables.[name]

    member internal this.ClearTemporaryVariables() =
        this.TempVariables.Clear()

    member this.GetActiveMemoryRegion() =
        _activeRegion.Value

    member this.GetPointerSize() =
        pointerSize

    member this.ReadNextInstruction() =      
        _stepEvent.Trigger(this)
        let instruction = this.GetInstruction()
        let programCounter = this.ProgramCounter
        this.Variables.[programCounter.Name] <- 
            {programCounter with
                Value = BitVector.add programCounter.Value (BitVector.ofUInt32 instruction.Length 32<rt>)
            }
        instruction

    member this.Step = _stepEvent.Publish 
    
    interface IProcessContainer with
        member this.ProgramCounter
            with get() = this.ProgramCounter

        member this.GetPointerSize() =
            this.GetPointerSize()

        member this.GetImportedFunctions() =
            this.GetImportedFunctions()

        member this.GetInstruction() =
            this.GetInstruction()

        member this.GetRegister(name: String) =
            this.GetRegister(name)

        member this.SetRegister(value: EmulatedValue) =
            this.SetRegister(value)

        member this.GetCallStack() =
            this.GetCallStack()
        
        member this.GetActiveMemoryRegion() =
            this.GetActiveMemoryRegion()

        [<CLIEvent>]
        member this.Step
            with get() = this.Step

        member this.Memory
            with get() = this.Memory

        member this.Cpu
            with get() = this.Cpu