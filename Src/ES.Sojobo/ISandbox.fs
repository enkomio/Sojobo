namespace ES.Sojobo

open System
open B2R2.BinIR
open System.Reflection

exception UnhandledFunction of string

type ISandbox =
    interface 
        /// Load the binary file name
        abstract Load: String -> unit

        /// Load a binary file represented by the input byte array
        abstract Load: Byte array -> unit  

        /// Start the execution of the process
        abstract Run: unit -> unit

        /// Stop the execution of the process
        abstract Stop: unit -> unit     

        /// Return the associated process with this sandbox
        abstract GetRunningProcess: unit -> IProcessContainer
        
        /// Add a library (in the form of Assembly) to the list of items
        /// to inspect to resolve function invocation. At runtime this Assembly will 
        /// analyzed to identify function with the followign signature:
        /// ISandbox -> CallbackResult
        /// It is also possible to specify additional parameters, like:
        /// ISandbox * param1:Int32 * param2:UInt32 -> CallbackResult
        /// The full name (namespace included) will be matched against the exported functions
        /// and if the binary will invoke it, the associated function will be invoked instead.
        abstract AddLibrary: Assembly-> unit

        /// This event is raised each time that an operation cause a side effect,
        // like the execution of an Interrupt or of a CPUIP instruction
        [<CLIEvent>]
        abstract SideEffect: IEvent<ISandbox * SideEffect> with get
    end