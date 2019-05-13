namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

[<AbstractClass>]
type BaseProcessContainer() =
    let mutable _activeRegion: MemoryRegion option = None

    member val internal Variables = new Dictionary<String, EmulatedValue>() with get
    member val internal TempVariables = new Dictionary<String, EmulatedValue>() with get

    member internal this.UpdateActiveMemoryRegion(memRegion: MemoryRegion) =
        _activeRegion <- Some memRegion

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = Utility.getTempName(index, emuType)
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
            let name = Utility.getTempName(name, emuType)
            this.TempVariables.[name]

    member internal this.ClearTemporaryVariables() =
        this.TempVariables.Clear()

    member this.GetActiveMemoryRegion() =
        _activeRegion.Value

    abstract GetProgramCounter: unit -> EmulatedValue
    abstract SetRegister: EmulatedValue -> unit
    abstract GetRegister: name: String -> EmulatedValue    
    abstract GetImportedFunctions: unit -> Symbol seq
    abstract GetInstruction: unit -> Instruction    
    abstract GetCallStack: unit -> UInt64 array
    abstract GetPointerSize: unit -> Int32    
    abstract Step: IEvent<IProcessContainer> with get
    abstract Memory: MemoryManager with get
    
    interface IProcessContainer with
        member this.GetProgramCounter() =
            this.GetProgramCounter()

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

        member this.Step
            with get() = this.Step

        member this.Memory
            with get() = this.Memory