# Sojobo - API documentation

_Sojobo_ provides a framework to analyze binaries. 

## Sandbox

The concept is that you create a Sandbox for each emulated process (1 thread only). So typically the first step is to create a Win32Sandbox object:

    var sandbox = new Win32Sandbox();
   
This object will emulate the process execution. You can use it to obtain information on the running process and its environment. Before to do so you have to load a binary inside the sandbox. This can be done by using one of the available ``Load`` method as in the following sample:

    sandbox.Load("malware.exe");
    
you can start the execution by executing the ``Run`` method:

    sandbox.Run();
    
### Add referenced libraries
By default _Sojobo_ include pre-compiled libraries that emulate the most used Windows functions. _Sojobo_ analyzed the import table and according to the imported libraries it sets the emulated function to be invoked. You can control this behavior with the ``Win32SandboxSettings`` object, that you can pass as parameter to the sandbox constructor. If the ``InitializeEnvironment`` is true (which is the default value), _Sojobo_ tries to load all default libraries in order to emulate the functions. If it sets to false you have to provide your own library.

Adding a library is very easy and can be done with the ``AddLibrary`` method. You can specify raw binaries or _Assemblies_. In order to intercept a function you have to follow a specific namespace pattern. For example to emulate the function _GetLastError_ from _Kernel32_ you have to create with name _Kernel32_ and function name _GetLastError_ as in the following example:

    public static class Kernel32
    {
        public static CallbackResult GetLastError(ISandbox sandbox)
        {
            var functionReturnValue = BitVectorFactory.Create(0x57);
            return new CallbackResult(functionReturnValue.ToOption(), CallingConvention.Cdecl);
        }
    }
    
The first parameter must be of type ``ISandbox``. You can specify other parameters, which value will be read from the stack. The type of the parameter must be ``Int32`` or ``UInt32``. Each emulated function must return a ``CallbackResult`` result object. Which specify a possible value to return and the _calling convention_ (this information is very important since allows the sandbox to know if it must clean the stack according to the number of parameters accepted by the function).

If you decide to add a simple library (like a native one), it will be mapped in the process address space and the ``PEB->Ldr`` value updated accordingly. 

_Sojobo_ supposes that the Library is invoked via a call, so before to invoke your emulated function it creates a new stack frame. It also destroy the stack frame when returns from the emulated function. This kind of hook are only placed by inspecting the IAT, so you cannot use this method to set an arbitrary hook on a function. To do that you have to use memory hook, explained below.

### Memory hook
Memory hooks allow to invoke your code once that the emulation reach a given address. You can place an hook by using the method ``AddHook``, as demonstrated in the following example:

    sandbox.AddHook("kernel32!VirtualFree", HookCallback);
    
where the signature of the ``HookCallback`` function is:
    
    public static void HookCallback(ISandbox sandbox)
    {
        // ...
    }
    
You can specify a symbol name (as in the example above) or just a numeric address.

## Process Container
Each emulated process is represented by a ``IProcessContainer``. You can obtain a reference to this object by invoking the method ``GetRunningProcess`` on the _sandbox_ object.

The _Process Container_ object allows you to access the registers and the memory address space. You can also setup an event handler in order to _Step_ its execution, as showed in the following example:

    var process = sandbox.GetRunningProcess();
    process.Step += ProcessStep;
    
where the ``ProcessStep`` function has the following signature:

    private static void ProcessStep(Object sender, IProcessContainer process)
    {
        // ...
    }
    
The ``IProcessContainer`` interface exports some useful methods to access various info. Below an excerpt of the interface:

    /// Get the memory manager associated with the process
    abstract Memory: MemoryManager with get

    /// Get the CPU object associated with the process
    abstract Cpu: Cpu with get

    /// Return the Pid valued of the emulated process
    abstract Pid: UInt32 with get

    /// Return the actual program counter value
    abstract ProgramCounter: EmulatedValue with get
        
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
    
## Memory Manager
It is possible to access the process memory via the _Memory Manager_. You can use it to reads or writes the process memory.

The memory manager was created in order to easy the reading of structures from memory. So if you want to read the _PEB_ structure (a rather complex structure) from memory, you don't need to read a buffer and parse it, you can just define the class (it is already defined in _Sojobo_ so don't need to do it) and read it, as showed in the following example:

    var peb = proc.Memory.ReadMemory<PEB32>(address);
    var dllname = peb.Ldr.InLoadOrderModuleList.FullDllName;
    
## Cpu
You can read specific registry value or set its value by using the ``Cpu`` object. In particular by using these two functions ``GetRegister`` and ``SetRegister``.

## C# binding
B2R2, Sojobo and all their tools are written in F#. This may cause some trouble to an C# developer, so I created a new project ``ES.Sojobo.CSharp`` that makes some tasks easier. If you plan to write your own tool in C# make sure to reference this library and use its extension methods or factory methods to created object that are not very friendly to create in C# (like the F# _Option_ type).

## Snapshot Manager
What a sandbox would be without a snapshot capability? :) 
_Sojobo_ allows to created sandbox snapshot by using the ``SnapshotManager`` object. You can also save the snapshot to filesystem or read a previously saved snapshot. This is very handy if you reach a point in the execution that must be analyzed more in depth.

For a full example on how to use the ``SnapshotManager`` take a look at <a href="https://github.com/enkomio/Sojobo/blob/master/Src/EndToEndTests/SnapshotTests.fs">this test</a>
