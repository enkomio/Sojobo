namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

[<AbstractClass>]
type BaseProcessContainer() =    
    member val internal Variables = new Dictionary<String, EmulatedValue>() with get
    member val internal TempVariables = new Dictionary<String, EmulatedValue>() with get

    member internal this.GetTempName(index: String, emuType: EmulatedType) =
        let size =  Utility.getSize(emuType)
        String.Format("T_{0}:{1}", index, size)   

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = this.GetTempName(index, emuType)
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
            let name = this.GetTempName(name, emuType)
            this.TempVariables.[name]

    member internal this.ClearTemporaryVariables() =
        this.TempVariables.Clear()

    abstract GetProgramCounter: unit -> EmulatedValue
    abstract GetProgramCounterValue: unit -> UInt64
    abstract GetArgument: position: Int32 -> EmulatedValue
    abstract SetVariable: EmulatedValue -> unit
    abstract GetVariable: name: String -> EmulatedValue
    abstract ReadMemory: address: UInt64 * size: Int32 -> Byte array
    abstract WriteMemory: UInt64 * Byte array -> unit
    abstract UpdateMemoryRegion: MemoryRegion * MemoryRegion -> unit
    abstract GetActiveMemoryRegion: unit -> MemoryRegion
    abstract GetMemoryRegion: UInt64 -> MemoryRegion
    abstract GetImportedFunctions: unit -> Symbol seq
    abstract GetInstruction: unit -> Instruction
    abstract Step: IEvent<IProcessContainer> with get
    abstract GetCallStack: unit -> UInt64 array
    abstract GetPointerSize: unit -> Int32

    interface IProcessContainer with
        member this.GetProgramCounter() =
            this.GetProgramCounter()

        member this.GetPointerSize() =
            this.GetPointerSize()

        member this.GetProgramCounterValue() =
            this.GetProgramCounterValue()

        member this.WriteMemory(address: UInt64, value: Byte array) =
            this.WriteMemory(address, value)

        member this.UpdateMemoryRegion(oldRegion: MemoryRegion, newRegion: MemoryRegion) =
            this.UpdateMemoryRegion(oldRegion, newRegion)

        member this.GetActiveMemoryRegion() =
            this.GetActiveMemoryRegion()

        member this.GetMemoryRegion(address: UInt64) =
            this.GetMemoryRegion(address)

        member this.GetImportedFunctions() =
            this.GetImportedFunctions()

        member this.GetArgument(position: Int32) =
            this.GetArgument(position)

        member this.ReadMemory(address: UInt64, size: Int32) =
            this.ReadMemory(address, size)

        member this.GetInstruction() =
            this.GetInstruction()

        member this.GetVariable(name: String) =
            this.GetVariable(name)

        member this.SetVariable(value: EmulatedValue) =
            this.SetVariable(value)

        member this.Step
            with get() = this.Step
    
        member this.GetCallStack() =
            this.GetCallStack()
