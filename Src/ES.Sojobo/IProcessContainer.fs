namespace ES.Sojobo

open System
open ES.Sojobo.Model
open B2R2.BinFile
open B2R2.FrontEnd

type IProcessContainer =
    interface 
        /// Get the memory manager associated with the process
        abstract Memory: MemoryManager with get

        /// This method is invoked before the next instruction being emulated
        abstract Step: IEvent<IProcessContainer> with get

        abstract GetProgramCounter: unit -> EmulatedValue
        abstract SetRegister: EmulatedValue -> unit
        abstract GetRegister: name: String -> EmulatedValue
        abstract GetActiveMemoryRegion: unit -> MemoryRegion
        abstract GetImportedFunctions: unit -> Symbol seq
        abstract GetInstruction: unit -> Instruction        
        abstract GetCallStack: unit -> UInt64 array
        abstract GetPointerSize: unit -> Int32        
    end