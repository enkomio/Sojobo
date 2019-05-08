namespace ES.Sojobo

open System
open ES.Sojobo.Model
open B2R2.BinFile
open B2R2.FrontEnd

type IProcessContainer =
    interface 
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
    end

