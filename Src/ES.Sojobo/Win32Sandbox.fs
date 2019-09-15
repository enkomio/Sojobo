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
open System.IO.Ports

[<CLIMutable>]
type Win32SandboxSettings = {
    /// This settings will initialize the environment by loading
    /// missing libraries or setup default hook for emulated functions
    InitializeEnvironment: Boolean

    /// If it is true, when the sandbox encounter a not handled exception
    /// before to exit it will save a snapshot of the system to filesystem
    SaveSnapshotOnException: Boolean

    /// If it is true, the lifted statements are cached to speed up emulation. This may cause
    /// wrong result if the emulated program write its own code (like malware)
    CacheInstructions: Boolean
} with
    static member Default = {
        InitializeEnvironment = true
        SaveSnapshotOnException = true
        CacheInstructions = true
    }

type Win32Sandbox(settings: Win32SandboxSettings) as this =
    inherit BaseSandbox()

    let _hooks = new Dictionary<UInt64, Hook>()
    let _cache = new InstructionCache()
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

    let removeEmptyLibraries() =
        getManagedLibraries(this.Libraries)
        |> Array.filter(fun lib -> lib.EmulatedMethods |> Seq.isEmpty)
        |> Array.iter(fun lib -> this.Libraries.Remove(Managed lib) |> ignore)
        
    let getAllExportedFunctions() =
        getNativeLibraries(this.Libraries)
        |> Array.filter(fun lib -> lib.Filename.IsSome)
        |> Array.collect(fun lib ->
            lib.Exports
            |> Seq.map(fun symbol ->
                let keyName = Helpers.getFunctionKeyName(symbol.Name, symbol.LibraryName)
                (keyName, symbol.Address)
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

    let loadNativeLibrary(lib: NativeLibrary) =
        _currentProcess
        |> Option.iter(fun p ->
            let baseAddress = lib.Load(this.GetRunningProcess())
            p.AddHandle(Library {Name = lib.GetLibraryName(); Value = baseAddress})
        )

    let loadNativeLibraries() =
        getNativeLibraries(this.Libraries)
        |> Seq.iter(loadNativeLibrary)
        
    let resolveHooks() =
        _hooks.Clear()
        this.Hooks
        |> Seq.iter(fun hook ->
            match hook with
            | Address (addr, callback) ->
                _hooks.[addr] <- hook
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
                            |> Seq.iter(fun symbol ->
                                if symbol.Name.Equals(functionName, StringComparison.OrdinalIgnoreCase) 
                                then _hooks.[symbol.Address] <- hook
                            )
                    | _ -> ()
                )
        ) 
        
    let initializeLibraries() =
        getManagedLibraries(this.Libraries)
        |> Seq.iter(fun lib -> lib.Initialize(this))

    let addLibrariesToSymbols() =
        getManagedLibraries(this.Libraries)
        |> Seq.collect(fun lib -> lib.EmulatedMethods |> Seq.map(fun m -> (lib, m)))
        |> Seq.map(fun (lib, kv) -> 
            try
                {
                    Name = kv.Value.Name
                    LibraryName = kv.Value.DeclaringType.Name
                    Address = lib.GetAddress(Helpers.getFunctionKeyName(kv.Value.Name, kv.Value.DeclaringType.Name))
                    Kind = SymbolKind.FunctionType
                    Target = TargetKind.DynamicSymbol
                } |> Some
            with _ -> None
        )
        |> Seq.choose id
        |> Seq.iter(this.GetRunningProcess().SetSymbol)
        
    let mapManagedLibraries() =
        resolveEmulatedFunctions()
        removeEmptyLibraries()
        mapEmulatedFunctions()
        initializeLibraries()
        addLibrariesToSymbols()

    let loadCoreLibrariesFromFilesystem() =
        Directory.GetFiles(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "*.dll")        
        |> Array.filter(fun dllFile -> Path.GetFileName(dllFile).StartsWith("ES.Sojobo"))
        |> Array.filter(fun dllFile -> dllFile.Equals(Assembly.GetExecutingAssembly().Location) |> not)
        |> Array.iter(fun dllFile ->
            try this.AddLibrary(Assembly.LoadFile(dllFile))
            with _ -> ()
        )

    let prepareForExecution() =
        if settings.InitializeEnvironment then
            loadCoreLibrariesFromFilesystem()
            
        // setup the native libraries
        loadNativeLibraries()

        // setup the emulated functions
        mapManagedLibraries()

        // setup hooks
        resolveHooks()

        // now that all libraries are mapped setup TEB and PEB
        setupTeb()

    let tryGetEmulationLibrary(proc: IProcessContainer) =  
        let programCounter = proc.ProgramCounter.Value |> BitVector.toUInt64   
        getManagedLibraries(this.Libraries)
        |> Array.sortByDescending(fun lib ->
            // this trick will load core libraries at the end, providing custom lib first
            if lib.GetAssembly().FullName.StartsWith("ES.Sojobo")
            then Int32.MinValue
            else Int32.MaxValue
        )
        |> Array.tryFind(fun lib -> lib.IsLibraryCall(programCounter))

    let invokeRegisteredHook(programCounter: UInt64) =        
        if _hooks.ContainsKey(programCounter) then 
            match _hooks.[programCounter] with
            | Address (_, callback) -> callback.Invoke(this)
            | Symbol (_, callback) -> callback.Invoke(this)            

    let emulateInstructionNoCache(proc: BaseProcessContainer, pc: UInt64) =
        let instruction = proc.GetInstruction()
        let handler = proc.GetActiveMemoryRegion().Handler
        (instruction, this.Emulator.Value.Emulate(handler, instruction))

    let emulateInstruction(proc: BaseProcessContainer, pc: UInt64) =
        _currentProcess.Value.SignalBeforeEmulation()
        if settings.CacheInstructions then
            if _cache.IsCached(pc) then 
                let (instruction, stmts) = _cache.GetCachedInstruction(pc)
                this.Emulator.Value.Emulate(stmts)
                this.Emulator.Value.AdvanceProgramCounterIfNecessary(instruction)
            else 
                let (instruction, stmts) = emulateInstructionNoCache(proc, pc)
                _cache.CacheInstruction(pc, instruction, stmts)
        else
            emulateInstructionNoCache(proc, pc) |> ignore
        _currentProcess.Value.SignalAfterEmulation()               

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

    let tryGetMappedLibrary(filename: String) =
        getNativeLibraries(this.Libraries) 
        |> Seq.tryFind(fun lib -> lib.Filename.Value.Equals(filename, StringComparison.OrdinalIgnoreCase))

    let run() =
        _stopExecution <- Some false
        while not _stopExecution.Value do
            // invoke hooks
            let pc = _currentProcess.Value.ProgramCounter.Value |> BitVector.toUInt64
            invokeRegisteredHook(pc)

            match tryGetEmulationLibrary(_currentProcess.Value) with
            | Some library -> library.InvokeLibraryFunction(this)
            | _ -> emulateInstruction(_currentProcess.Value, pc)
            
    new() = new Win32Sandbox(Win32SandboxSettings.Default) 
    
    default this.GetHookAddress(hook: Hook) =
        _hooks
        |> Seq.tryFind(fun kv -> kv.Value = hook)
        |> Option.map(fun kv -> kv.Key)
        
    override this.AddHook(symbol: String, callback: Action<ISandbox>) =
        let hook = base.AddHook(symbol, callback)
        match _stopExecution with
        | Some _ -> resolveHooks()
        | _ -> ()
        hook

    override this.AddHook(address: UInt64, callback: Action<ISandbox>) =
        let hook = base.AddHook(address, callback)
        _stopExecution |> Option.iter(fun _ -> resolveHooks()) 
        hook

    override this.RemoveHook(hook: Hook) =
        base.RemoveHook(hook)
        resolveHooks()

    override this.AddLibrary(filename: String) =
        match tryGetMappedLibrary(filename) with
        | Some _ -> ()
        | None ->
            base.AddLibrary(filename)
            _stopExecution
            |> Option.iter(fun _ ->
                let library = tryGetMappedLibrary(filename).Value
                loadNativeLibrary(library)
                mapManagedLibraries()
            )            

    default this.Run() =
        // initialize structures and hooks    
        prepareForExecution()
                        
        // start execution loop in a try catch to catch exception
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

    default this.ResetProcessState() =
        match _currentProcess with
        | Some proc -> proc.ResetState()
        | _ ->_currentProcess <- new Win32ProcessContainer() |> Some
        
    default this.Load(filename: String) =
        this.ResetProcessState()
        _currentProcess.Value.Initialize(filename)
        loadReferencedLibraries()

    default this.Load(buffer: Byte array) =
        this.ResetProcessState()
        _currentProcess.Value.Initialize(buffer)
        loadReferencedLibraries()

    default this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer

    