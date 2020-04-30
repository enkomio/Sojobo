namespace ES.Sojobo.Windows

open System
open System.IO
open System.Collections.Generic
open System.Reflection
open B2R2
open B2R2.FrontEnd.Intel
open B2R2.BinFile
open B2R2.FrontEnd
open B2R2.BinFile.PE
open ES.Sojobo
open ES.Sojobo.Model
open WindowsStructures

[<CLIMutable>]
type WindowsSandboxSettings = {
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

type WindowsSandbox(pointerSize: Int32, settings: WindowsSandboxSettings) as this =
    inherit BaseSandbox()

    let _hooks = new Dictionary<UInt64, Hook>()
    let _cache = new InstructionCache()
    let mutable _currentProcess: WindowsProcessContainer option = None
    let mutable _stopExecution: Boolean option = None    
    do this.Emulator <- Some(upcast new LowUIREmulator(this))

    let setupTeb() =
        let tebAddress = 
            if this.GetRunningProcess().PointerSize = 32 then
                StructuresFactory.createTeb32(this)
            else
                StructuresFactory.createTeb64(this)

        if this.GetRunningProcess().PointerSize = 32 then [
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)        
        ] else [
            createVariableWithValue(string Register.GSBase, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)
            createVariableWithValue(string Register.GS, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)        
        ] 
        |> List.iter(this.GetRunningProcess().Cpu.SetRegister)

    let resolveEmulatedFunctions() =
        this.ApiEmulators
        |> Seq.iter(fun lib -> lib.ResolveLibraryFunctions())

    let removeEmptyLibraries() =
        this.ApiEmulators
        |> Seq.toArray
        |> Array.filter(fun lib -> lib.EmulatedMethods |> Seq.isEmpty)
        |> Array.iter(fun lib -> this.ApiEmulators.Remove(lib) |> ignore)
        
    let getAllExportedFunctions() =
        this.NativeLibraries
        |> Seq.toArray
        |> Array.filter(fun lib -> lib.FileName.IsSome)
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
        this.ApiEmulators
        |> Seq.iter(fun lib ->
            lib.MapSymbolWithManagedMethods(this.GetRunningProcess(), exportedFunctions)
        )

    let loadNativeLibrary(lib: NativeLibrary) =
        _currentProcess
        |> Option.iter(fun p ->
            lib.Load(this.GetRunningProcess())            
            p.AddHandle(Library lib.Handle)
        )
                
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
                this.NativeLibraries
                |> Seq.iter(fun lib ->
                    let filename = (Path.GetFileName <| lib.FileName.Value.ToLowerInvariant()).Replace(".dll", String.Empty).Trim()
                    if moduleName.Equals(filename, StringComparison.OrdinalIgnoreCase) then
                        // try to identify an exported function with the same name
                        lib.Exports
                        |> Seq.iter(fun symbol ->
                            if symbol.Name.Equals(functionName, StringComparison.OrdinalIgnoreCase) 
                            then _hooks.[symbol.Address] <- hook
                    )
                )
        ) 
        
    let initializeLibraries() =
        this.ApiEmulators
        |> Seq.iter(fun lib -> lib.Initialize(this))

    let addLibrariesToSymbols() =
        this.ApiEmulators
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
            try this.AddApiEmulator(Assembly.LoadFile(dllFile))
            with _ -> ()
        )

    let prepareForExecution() =        
        loadCoreLibrariesFromFilesystem()
            
        // setup the emulated functions
        mapManagedLibraries()

        // setup hooks
        resolveHooks()

        // now that all libraries are mapped setup TEB and PEB
        setupTeb()

    let tryGetEmulationLibrary(proc: IProcessContainer) =  
        let programCounter = proc.ProgramCounter.Value |> BitVector.toUInt64   
        this.ApiEmulators
        |> Seq.toArray
        |> Array.sortByDescending(fun lib ->
            // this trick will load core libraries at the end, providing custom lib first
            if lib.GetAssembly().FullName.StartsWith("ES.Sojobo", StringComparison.OrdinalIgnoreCase)
            then Int32.MinValue
            else Int32.MaxValue
        )
        |> Array.tryFind(fun lib -> lib.IsLibraryCall(programCounter))

    let invokeRegisteredHook(programCounter: UInt64) =        
        if _hooks.ContainsKey(programCounter) then 
            match _hooks.[programCounter] with
            | Address (_, callback) -> callback.Invoke(this)
            | Symbol (_, callback) -> callback.Invoke(this)            

    let emulateInstructionNoCache(proc: BaseProcessContainer) =
        let instruction = proc.GetInstruction()
        let handler = proc.GetActiveMemoryRegion().Handler
        (instruction, this.Emulator.Value.Emulate(handler, instruction))

    let emulateInstruction(proc: BaseProcessContainer, pc: UInt64) =
        this.SignalBeforeEmulation()
        if settings.CacheInstructions then
            if _cache.IsCached(pc) then 
                let (instruction, stmts) = _cache.GetCachedInstruction(pc)
                this.Emulator.Value.Emulate(stmts)
                this.Emulator.Value.AdvanceProgramCounterIfNecessary(instruction)
            else 
                let (instruction, stmts) = emulateInstructionNoCache(proc)
                _cache.CacheInstruction(pc, instruction, stmts)
        else
            emulateInstructionNoCache(proc) |> ignore
        this.SignalAfterEmulation()               

    let rec loadNativeLibraryFile(filename: String, loadedLibraries: HashSet<String>) =
        let libPath = 
            if _currentProcess.Value.PointerSize = 32 then Environment.SpecialFolder.SystemX86
            else Environment.SpecialFolder.System
            |> Environment.GetFolderPath

        let libName = Path.Combine(libPath, filename)
        if File.Exists(libName) && loadedLibraries.Add(libName.ToLowerInvariant()) then
            this.MapLibrary(libName)

            // load also all referenced DLL
            let isa = _currentProcess.Value.GetActiveMemoryRegion().Handler.ISA
            let handler = BinHandler.Init(isa, libName)
            Helpers.getPe(handler).ImportMap 
            |> Seq.map(fun kv -> 
                match kv.Value with
                | ImportByOrdinal (_, dllname) -> dllname
                | ImportByName (_, _, dllname) -> dllname
            )
            |> Seq.iter(fun dllName -> loadNativeLibraryFile(dllName, loadedLibraries))

    let loadReferencedLibraries() =        
        let loadedLibraries = new HashSet<String>()
        _currentProcess.Value.GetImportedFunctions()
        |> Seq.distinctBy(fun symbol -> symbol.LibraryName)
        |> Seq.map(fun lib -> lib.LibraryName)
        |> Seq.iter(fun libName -> loadNativeLibraryFile(libName, loadedLibraries))

    let tryGetMappedLibrary(fileName: String) =
        let name = Path.GetFileName(fileName)
        this.NativeLibraries
        |> Seq.tryFind(fun lib -> lib.Name.Value.Equals(name, StringComparison.OrdinalIgnoreCase))

    let run() =
        _stopExecution <- Some false
        while not _stopExecution.Value do
            // invoke hooks
            let pc = _currentProcess.Value.ProgramCounter.Value |> BitVector.toUInt64
            invokeRegisteredHook(pc)

            match tryGetEmulationLibrary(_currentProcess.Value) with
            | Some library -> library.InvokeLibraryFunction(this)
            | _ -> emulateInstruction(_currentProcess.Value, pc)
            
    new(pointerSize: Int32) = new WindowsSandbox(pointerSize, WindowsSandboxSettings.Default) 
    
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

    override this.MapLibrary(filename: String) =
        match tryGetMappedLibrary(filename) with
        | Some _ -> ()
        | None ->
            base.MapLibrary(filename)
            
            tryGetMappedLibrary(filename)
            |> Option.iter(fun lib ->
                loadNativeLibrary(lib)

                _stopExecution
                |> Option.iter(fun stopExecution ->
                    // if the process is currently emulated execute pre-init operations
                    if not stopExecution then 
                        // map library in order to place hooks for 
                        // functions exported from the just loaded lib
                        mapManagedLibraries()

                        // update PEB Ldr field by setup it again (yeah not very efficient)
                        let tebAddress = this.GetRunningProcess().Cpu.GetRegister(string Register.FSBase).As<UInt64>() 
                        this.GetRunningProcess().Memory.FreeMemoryRegion(tebAddress) |> ignore
                        setupTeb()
                )        
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
        | _ -> _currentProcess <- new WindowsProcessContainer(pointerSize) |> Some
        
    default this.Load(filename: String) =
        this.ResetProcessState()
        _currentProcess.Value.Initialize(filename)
        if settings.InitializeEnvironment then
            loadReferencedLibraries()

    default this.Load(buffer: Byte array) =
        this.ResetProcessState()
        _currentProcess.Value.Initialize(buffer)
        if settings.InitializeEnvironment then
            loadReferencedLibraries()

    default this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer

    