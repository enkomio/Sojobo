namespace ES.Sojobo

open System
open System.Reflection
open B2R2
open B2R2.FrontEnd.Intel
open ES.Sojobo.Win32
open ES.Sojobo.Model

type Win32SandboxSettings = {
    /// This settings will initialize the environment by loading
    /// missing libraries or setup default hook for emulated functions
    InitializeEnvironment: Boolean
} with
    static member Default = {
        InitializeEnvironment = true
    }

type Win32Sandbox(settings: Win32SandboxSettings) as this =
    inherit BaseSandbox()

    let mutable _stopExecution = false
    let mutable _currentProcess: Win32ProcessContainer option = None
    do this.Emulator <- Some(upcast new LowUIREmulator(this))

    let setupTeb() =
        let tebAddress = createTeb(this)
        if this.GetRunningProcess().GetPointerSize() = 32 then [
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)        
        ] else [
            createVariableWithValue(string Register.FSBase, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)        
        ] 
        |> List.iter(this.GetRunningProcess().SetRegister)

    let resolveEmulatedFunctions() =
        this.Libraries
        |> Seq.iter(fun lib ->
            match lib with
            | Managed lib -> lib.ResolveLibraryFunctions()
            | _ -> ()
        )

    let mapEmulatedFunctions() =
        this.Libraries
        |> Seq.iter(fun lib ->
            match lib with
            | Managed lib -> 
                let proc = this.GetRunningProcess()
                lib.MapSymbolWithManagedFunctions(proc.Memory, proc.GetImportedFunctions())
            | _ -> ()
        )

    let mapNativeLibraries() =
        this.Libraries
        |> Seq.iter(fun lib ->
            match lib with
            | Native lib -> 
                try
                    let assembly = Assembly.Load(lib.Content)
                    this.AddLibrary(assembly)
                with 
                    | :? BadImageFormatException ->
                        lib.Load(this.GetRunningProcess())
            | _ -> ()
        )

    let prepareForExecution() =
        if settings.InitializeEnvironment then
            this.AddLibrary(Assembly.GetExecutingAssembly())

        // setup the native libraries
        mapNativeLibraries()

        // setup the emulated functions
        resolveEmulatedFunctions()
        mapEmulatedFunctions()

        // now that all libraries are mapped setup TEB and PEB
        setupTeb()

    let tryGetEmulationLibrary(proc: IProcessContainer) =
        let programCounter = proc.GetProgramCounter().Value |> BitVector.toUInt64        
        this.Libraries
        |> Seq.tryFind(fun lib ->
            match lib with
            | Managed lib -> lib.IsLibraryCall(programCounter)
            | _ -> false
        )

    let emulateNextInstruction(proc: BaseProcessContainer, endAddress: UInt64) =
        let instruction = proc.ReadNextInstruction()
        let handler = proc.GetActiveMemoryRegion().Handler
        this.Emulator.Value.EmulateInstruction(handler, instruction)

        // check ending condition
        _stopExecution <- 
            _stopExecution || 
            proc.GetProgramCounter().Value |> BitVector.toUInt64 >= endAddress
            
    new() = new Win32Sandbox(Win32SandboxSettings.Default)

    default this.AddHook(address: UInt64, callback: Action<ISandbox>) =
        ()

    default this.AddHook(symbol: String, callback: Action<ISandbox>) =
        ()

    default this.Run() =            
        let win32Process = _currentProcess.Value
        let activeRegion = win32Process.GetActiveMemoryRegion()
        let endAddress = activeRegion.BaseAddress + uint64 activeRegion.Content.Length
        
        // initialize structures and hooks    
        prepareForExecution()
                        
        // start execution loop
        _stopExecution <- false
        while not _stopExecution do
            match tryGetEmulationLibrary(win32Process) with
            | Some (Managed library) -> library.InvokeLibraryFunction(this)
            | _ -> emulateNextInstruction(win32Process, endAddress)

    default this.Stop() =
        _stopExecution <- true
        
    default this.Load(filename: String) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(filename)

    default this.Load(buffer: Byte array) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(buffer)

    default this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer