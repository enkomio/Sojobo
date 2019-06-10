# Sojobo - API documentation

_Sojobo_ provides a framework to analyze binaries. 

## Sandbox

The concept is that you create a Sandbox for each emulated process (1 thread only). So tipically the first step is to create a Win32Sandbox object:

    var sandbox = new Win32Sandbox();
   
This object will emulate the process execution. You can use it to obtain information on the running process and its environment. Before to do so you have to load a binary inside the sandbox. This can be done by using one of the available ``Load`` method as in the following sample:

    sandbox.Load("malware.exe");
    
you can start the execution by executing the ``Run`` method:

    sandbox.Run();
    
### Add referenced libraries
By default _Sojobo_ include pre-compiled libraries that emulate the most used Windows functions. _Sojobo_ nalyzed the import table and according to the imported libraries it sets the emulated function to be invoked. You can control this behaviour with the ``Win32SandboxSettings`` object, that you can pass as parameter to the sandbox constructor. If the ``InitializeEnvironment`` is true (which is the default value), _Sojobo_ tries to load all default libraries in order to emulate the functions. If it sets to false you have to provide your own library.

Addind a library is very easy anc can be done with the ``AddLibrary`` method. You can specify raw binaries or _Assemblies_. In order to intercept a function you have to follow a specific namespace pattern. For example to emulate the function _GetLastError_ from _Kernel32_ you have to create with name _Kernel32_ and function name _GetLastError_ as in the following example:

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