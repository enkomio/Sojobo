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
        [<CLIEvent>]
        abstract Step: IEvent<IProcessContainer> with get

        /// Return the actual program counter value
        abstract GetProgramCounter: unit -> EmulatedValue

        /// set a specific register
        abstract SetRegister: EmulatedValue -> unit

        /// get the value of the specified register
        abstract GetRegister: name: String -> EmulatedValue

        /// get the memory region that is currenlty being executed by the process
        abstract GetActiveMemoryRegion: unit -> MemoryRegion

        /// get a list of symbols that are imported by the binary
        abstract GetImportedFunctions: unit -> Symbol seq

        /// get the next instruction that is going to be executed
        abstract GetInstruction: unit -> Instruction        

        /// Return an array of addresses related to the current call stack
        /// The array is composed by walking the stack, if it corrupted, this 
        /// value will be corrupted too
        abstract GetCallStack: unit -> UInt64 array

        /// Get the size in bit of the pointer for the current process
        abstract GetPointerSize: unit -> Int32        
    end