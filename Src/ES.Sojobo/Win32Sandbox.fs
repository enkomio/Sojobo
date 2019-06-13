namespace ES.Sojobo

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open B2R2
open B2R2.FrontEnd.Intel
open B2R2.BinFile
open B2R2.FrontEnd
open B2R2.BinFile.PE
open ES.Sojobo.Win32
open ES.Sojobo.Model

[<CLIMutable>]
type Win32SandboxSettings = {
    /// This settings will initialize the environment by loading
    /// missing libraries or setup default hook for emulated functions
    InitializeEnvironment: Boolean

    /// If it is true, when the sandbox encounter a not handled exception
    /// before to exit it will save a snapshot of the system to filesystem
    SaveSnapshotOnException: Boolean
} with
    static member Default = {
        InitializeEnvironment = true
        SaveSnapshotOnException = true
    }

type Win32Sandbox(settings: Win32SandboxSettings) as this =
    inherit BaseSandbox()

    let _hooks = new Dictionary<UInt64, Action<ISandbox>>()
    let mutable _stopExecution: Boolean option = None
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
        |> List.iter(this.GetRunningProcess().Cpu.SetRegister)

    let resolveEmulatedFunctions() =
        getManagedLibraries(this.Libraries)
        |> Seq.iter(fun lib -> lib.ResolveLibraryFunctions())

    let getAllExportedFunctions() =
        getNativeLibraries(this.Libraries)
        |> Array.filter(fun lib -> lib.Filename.IsSome)
        |> Array.collect(fun lib ->
            lib.Exports
            |> Seq.map(fun kv ->
                let keyName = Helpers.getFunctionKeyName(kv.Value, lib.Filename.Value |> Path.GetFileName)
                (keyName, kv.Key)
            )
            |> Seq.toArray
        )
        |> dict

    let mapEmulatedFunctions() =
        let exportedFunctions = getAllExportedFunctions()
        getManagedLibraries(this.Libraries)
        |> Seq.iter(fun lib ->
            let proc = this.GetRunningProcess()
            lib.MapSymbolWithManagedMethods(proc.Memory, proc.GetImportedFunctions(), exportedFunctions)
        )

    let mapNativeLibraries() =
        getNativeLibraries(this.Libraries)
        |> Seq.iter(fun lib ->
            try
                let assembly = Assembly.Load(lib.Content)
                this.AddLibrary(assembly)
            with 
                | :? BadImageFormatException ->
                    lib.Load(this.GetRunningProcess())
        )

    let resolveHooks() =
        this.Hooks
        |> Seq.iter(function
            | Address (addr, callback) ->
                _hooks.[addr] <- callback
            | Symbol (symbol, callback) ->
                let items = symbol.ToLowerInvariant().Replace(".dll", String.Empty).Split([|'!'|])
                let (moduleName, functionName) = (items.[0].Trim(), items.[1].Trim())
                this.Libraries
                |> Seq.iter(function 
                    | Native lib when lib.Filename.IsSome -> 
                        let filename = (Path.GetFileName <| lib.Filename.Value.ToLowerInvariant()).Replace(".dll", String.Empty).Trim()
                        if moduleName.Equals(filename, StringComparison.OrdinalIgnoreCase) then
                            // try to identify an exported function with the same name
                            lib.Exports
                            |> Seq.iter(fun kv ->
                                if kv.Value.Equals(functionName, StringComparison.OrdinalIgnoreCase) then
                                    _hooks.[kv.Key] <- callback
                            )
                    | _ -> ()
                )
        )   
        
    let mapManagedLibraries() =
        resolveEmulatedFunctions()
        mapEmulatedFunctions()

    let loadProjectLibrariesFromFilesystem() =
        Directory.GetFiles(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "*.dll")        
        |> Array.filter(fun dllFile -> Path.GetFileName(dllFile).StartsWith("ES.Sojobo"))
        |> Array.filter(fun dllFile -> dllFile.Equals(Assembly.GetExecutingAssembly().Location) |> not)
        |> Array.iter(fun dllFile ->
            try this.AddLibrary(Assembly.LoadFile(dllFile))
            with _ -> ()
        )

    let prepareForExecution() =
        if settings.InitializeEnvironment then
            loadProjectLibrariesFromFilesystem()

        // setup the native libraries
        mapNativeLibraries()

        // setup the emulated functions
        mapManagedLibraries()

        // setup hooks
        resolveHooks()

        // now that all libraries are mapped setup TEB and PEB
        setupTeb()

    let tryGetEmulationLibrary(proc: IProcessContainer) =
        let programCounter = proc.ProgramCounter.Value |> BitVector.toUInt64     
        getManagedLibraries(this.Libraries)
        |> Seq.tryFind(fun lib -> lib.IsLibraryCall(programCounter))

    let invokeRegisteredHook(programCounter: UInt64) =        
        if _hooks.ContainsKey(programCounter) 
        then _hooks.[programCounter].Invoke(this)

    let emulateNextInstruction(proc: BaseProcessContainer, programCounter: UInt64) =
        let instruction = proc.ReadNextInstruction()
        let handler = proc.GetActiveMemoryRegion().Handler

        invokeRegisteredHook(programCounter)

        this.Emulator.Value.EmulateInstruction(handler, instruction)

    let rec loadLibraryFile(filename: String, loadedLibraries: HashSet<String>) =
        let libPath = Environment.GetFolderPath(Environment.SpecialFolder.SystemX86)
        let libName = Path.Combine(libPath, filename)
        if File.Exists(libName) && loadedLibraries.Add(libName.ToLowerInvariant()) then
            this.AddLibrary(libName)

            // load also all referenced DLL
            let handler = BinHandler.Init(ISA.OfString "x86", libName)
            Helpers.getPe(handler).ImportMap 
            |> Seq.map(fun kv -> 
                match kv.Value with
                | ImportByOrdinal (_, dllname) -> dllname
                | ImportByName (_, _, dllname) -> dllname
            )
            |> Seq.iter(fun dllName -> loadLibraryFile(dllName, loadedLibraries))

    let loadReferencedLibraries() =
        if settings.InitializeEnvironment then
            let loadedLibraries = new HashSet<String>()
            _currentProcess.Value.GetImportedFunctions()
            |> Seq.distinctBy(fun symbol -> symbol.LibraryName)
            |> Seq.map(fun lib -> lib.LibraryName)
            |> Seq.iter(fun libName -> loadLibraryFile(libName, loadedLibraries))

    let run() =
        _stopExecution <- Some false
        while not _stopExecution.Value do
            let programCounter = _currentProcess.Value.ProgramCounter.Value |> BitVector.toUInt64
            invokeRegisteredHook(programCounter)

            match tryGetEmulationLibrary(_currentProcess.Value) with
            | Some library -> library.InvokeLibraryFunction(this)
            | _ -> emulateNextInstruction(_currentProcess.Value, programCounter)
            
    new() = new Win32Sandbox(Win32SandboxSettings.Default)   
    
    override this.AddLibrary(filename: String) =
        base.AddLibrary(filename)
        match _stopExecution with
        | Some _ ->
            // process is running, map the library in memory too
            let library = 
                getNativeLibraries(this.Libraries) 
                |> Seq.find(fun lib -> lib.Filename.Value.Equals(filename, StringComparison.Ordinal))
            
            // load native library
            library.Load(this.GetRunningProcess())
            
            // map emulated function too            
            mapManagedLibraries()
        | _ -> ()

    default this.Run() =
        // initialize structures and hooks    
        prepareForExecution()
                        
        // start execution loop in a try catch to catch expection
        try run()
        with _ ->
            if settings.SaveSnapshotOnException then
                let pc = this.GetRunningProcess().ProgramCounter.Value |> BitVector.toInt32
                let snapshotManager = new SnapshotManager(this)
                let snapshot = snapshotManager.TakeSnaphot()
                let filename = String.Format("crashdump_0x{0}_{1}.dump", pc, DateTime.UtcNow.ToString("yyyyMMdd"))
                snapshot.SaveTo(filename)
            reraise()

    default this.Stop() =
        _stopExecution <- Some true

    default this.CreateEmptyProcess() =
        _currentProcess <- new Win32ProcessContainer() |> Some
        
    default this.Load(filename: String) =
        this.CreateEmptyProcess()
        _currentProcess.Value.Initialize(filename)
        loadReferencedLibraries()

    default this.Load(buffer: Byte array) =
        this.CreateEmptyProcess()
        _currentProcess.Value.Initialize(buffer)
        loadReferencedLibraries()

    default this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer

    