namespace ES.Sojobo

open System
open B2R2.BinIR
open System.Reflection
open ES.Sojobo.Model

exception UnhandledFunction of string

type ISandbox =
    interface 
        /// Sandbox Id
        abstract Id: Guid with get

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
        /// be analyzed to identify functions with the following signature:
        /// ISandbox -> CallbackResult
        /// It is also possible to specify additional parameters, like:
        /// ISandbox * param1:Int32 * param2:UInt32 -> CallbackResult
        /// The full name (namespace included) will be matched against the exported functions
        /// and if the binary will invoke it, the associated function will be invoked instead.
        abstract AddApiEmulator: Assembly -> unit

        /// Add the content of the parameter as a library. The content will be mapped into
        /// the process address space and the exported functions resolved in order to be 
        /// emulated
        abstract MapLibrary: content:Byte array -> unit
        
        /// Add the content of the file as a library. 
        /// Its content will be mapped with the same process of the
        /// method to add a Byte array
        abstract MapLibrary: filename:String -> unit

        /// Add an hook at the specified address. The callback is invoked before the 
        /// instruction at that address is emulated
        abstract AddHook: address:UInt64 * callback:Action<ISandbox> -> Hook

        /// Add an hook at the specified symbol. A symbol is represented by the format:
        /// <module name>!<function name>. For example to add an hook when VirtualAlloc is 
        /// invoked, use: kernel32!VirtualAlloc.
        /// The callback is invoked before the instruction at that address is emulated
        abstract AddHook: symbol:String * callback:Action<ISandbox> -> Hook
        
        /// Remove a previously added hook
        abstract RemoveHook: hook:Hook -> unit

        /// Get an array of all placed hooks
        abstract GetHooks: unit -> Hook array

        /// Get the address of the hook after that it was mapped
        abstract GetHookAddress: Hook -> UInt64 option

        /// This event is raised each time that an operation cause a side effect,
        // like the execution of an Interrupt or of a CPUIP instruction
        [<CLIEvent>]
        abstract SideEffect: IEvent<ISandbox * SideEffect> with get

        /// This method is invoked before the instruction being emulated
        [<CLIEvent>]
        abstract BeforeEmulation: IEvent<IProcessContainer> with get

        /// This method is invoked after the instruction being emulated
        [<CLIEvent>]
        abstract AfterEmulation: IEvent<IProcessContainer> with get

        /// Return the istance of the emulator used to emulate the LowUIR code
        abstract Emulator: IEmulator with get
    end

and Hook =
    | Address of address:UInt64 * callback:Action<ISandbox>
    | Symbol  of symbol:String * callback:Action<ISandbox>