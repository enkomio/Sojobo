namespace ES.Sojobo

open System
open ES.Sojobo.Model
open B2R2.BinFile

type IProcessContainer =
    interface 
        abstract GetProgramCounter: unit -> EmulatedValue
        abstract GetProgramCounterValue: unit -> UInt64
        abstract GetArgument: position: Int32 -> EmulatedValue
        abstract ReadMemory: address: UInt64 * size: Int32 -> Byte array
        abstract WriteMemory: UInt64 * Byte array -> unit
        abstract UpdateMemoryRegion: MemoryRegion * MemoryRegion -> unit
        abstract GetActiveMemoryRegion: unit -> MemoryRegion
        abstract GetMemoryRegion: UInt64 -> MemoryRegion
        abstract GetImportedFunctions: unit -> Symbol seq
    end

