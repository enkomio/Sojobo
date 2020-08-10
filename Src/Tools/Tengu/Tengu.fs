namespace ES.Tengu

open System
open System.Reflection
open System.IO
open System.Text
open B2R2
open ES.Fslog
open ES.Fslog.Loggers
open ES.Sojobo
open ES.Sojobo.Windows
open ES.Tengu.Cli
open B2R2.FrontEnd

type Tengu(settings: Settings) =
    let _sandboxSettings = {WindowsSandboxSettings.Default with InitializeEnvironment = true}
    let mutable _sandbox = new WindowsSandbox(32, _sandboxSettings)
    let mutable _debugger = new Debugger(_sandbox)
    let mutable _dumper = new Dumper(_sandbox)
    let mutable _metrics = new Metrics(_sandbox)
    let mutable _instructionCounter = 0
    
    let _logger =
        log "Tengu"
        |> info "Start" "-=[ Start Analysis ]=-"
        |> info "Details" "File: {0} PID: {1}"
        |> info "Completed" "-=[ Analysis Completed ]=-"
        |> info "SnapshotSaved" "Sandbox snapshot saved to: {0}"
        |> info "LoadLibrary" "Loaded native library to map: {0}"
        |> info "LoadApiLibrary" "Loaded API Emulation library: {0}"
        |> info "SnapshotLoaded" "Loaded snapshot from: {0}"
        |> info "EmulatedInstructions" "Number of emulated instructions: {0}"
        |> warning "SnapshotNotFound" "Snapshot file '{0}' not found, ignore loading."
        |> error "Exception" "PC: {0} - Error: {1}"
        |> build    

    let beforeEmulationEventHandler(settings: Settings) (proc: IProcessContainer) =
        _debugger.BeforeEmulation()
        
    let initialize() =
        let handler = BinHandler.Init(ISA.DefaultISA, settings.Filename)
        
        // create 64 bit objects
        if handler.FileInfo.WordSize = WordSize.Bit64 then
            _sandbox <- new WindowsSandbox(64, _sandboxSettings)
            _debugger <- new Debugger(_sandbox)
            _dumper <- new Dumper(_sandbox)
            _metrics <- new Metrics(_sandbox)

        // load file in sandbox
        _sandbox.Load(settings.Filename)

        // setup handlers
        _sandbox.BeforeEmulation.Add(beforeEmulationEventHandler settings)

        // add this file as library for method hooking
        _sandbox.AddApiEmulator(typeof<Dumper>.Assembly)

        // add all api emulators
        settings.ApiLibs 
        |> Array.iter(fun lib -> 
            _logger?LoadApiLibrary(lib)
            let libContent = File.ReadAllBytes(lib)
            _sandbox.AddApiEmulator(Assembly.Load(libContent))
        )

        // add all the specified input libraries
        settings.Libs 
        |> Array.iter(fun lib -> 
            _logger?LoadLibrary(lib)
            _sandbox.MapLibrary(lib)
        )

        // configure debugger
        _debugger.PrintDisassembly <- settings.PrintDisassembly
        _debugger.PrintIR <- settings.PrintIR

    let runSample() =
        try
            _logger?Start()
            _logger?Details(settings.Filename, _sandbox.GetRunningProcess().Pid)

            // run debugger
            _debugger.Start()
            if settings.Break then _debugger.Break()

            // run the sample till the end or exception
            _sandbox.Run()
            _logger?Completed()
            true
        with e ->
            // Exception due to some limitation in this emulator
            _logger?Exception(_sandbox.GetRunningProcess().ProgramCounter.Value, e)
            false        
                    
    let configureLogging(logLevel: LogLevel) =
        let logProvider = new LogProvider()
        logProvider.AddLogger(new ConsoleLogger(logLevel))
        let logFile = Path.Combine(Utility.getResultDir(_sandbox.GetRunningProcess().Pid), "output.log")
        logProvider.AddLogger(new FileLogger(logLevel, logFile))
        logProvider.AddLogSourceToLoggers(_logger)

        // service loggers
        _dumper.ConfigureLogger(logProvider)
        _metrics.ConfigureLogger(logProvider)

    let saveSnapshot() =
        if settings.SaveSnapshotOnExit then
            let snapshotManager = new SnapshotManager(_sandbox)
            snapshotManager.TakeSnaphot().SaveTo(settings.SnapshotToSave)
            _logger?SnapshotSaved(settings.SnapshotToSave)

    let loadSnapshot() =
        if settings.LoadSnapshotOnStart then
            if File.Exists(settings.SnapshotToLoad) then
                let snapshotManager = new SnapshotManager(_sandbox)
                let snapshot = Model.Snapshot.Read(settings.SnapshotToLoad)
                snapshotManager.LoadSnapshot(snapshot)
                _logger?SnapshotLoaded(settings.SnapshotToLoad)
            else
                _logger?SnapshotNotFound(settings.SnapshotToLoad)

    member this.Run() =
        initialize()
        configureLogging(LogLevel.Informational)        
        loadSnapshot()

        if runSample() then
            // the emulation ended correctly
            _dumper.SaveInformation()
            _metrics.SaveInformation()
            saveSnapshot()
        _logger?EmulatedInstructions(_instructionCounter)