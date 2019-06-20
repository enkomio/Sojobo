namespace ES.Tengu

open System
open System.Reflection
open System.IO
open System.Text
open B2R2
open ES.Fslog
open ES.Fslog.Loggers
open ES.Sojobo
open ES.Tengu.Cli

module Program =
    let private _sandbox = new Win32Sandbox()
    let private _debugger = new Debugger(_sandbox)
    let private _dumper = new Dumper(_sandbox)
    let private _metrics = new Metrics(_sandbox)
    let mutable private _instructionCounter = 0

    let private _logger =
        log "Tangu"
        |> info "Start" "-=[ Start Analysis ]=-"
        |> info "Details" "File: {0} PID: {1}"
        |> info "Completed" "-=[ Analysis Completed ]=-"
        |> info "SnapshotSaved" "Sandbox snapshot saved to: {0}"
        |> info "LoadLibrary" "Loaded library: {0}"
        |> info "SnapshotLoaded" "Loaded snapshot from: {0}"
        |> info "EmulatedInstructions" "Number of emulated instructions: {0}"
        |> warning "SnapshotNotFound" "Snapshot file '{0}' not found, ignore loading."
        |> error "Exception" "PC: {0} - Error: {1}"
        |> build    

    let private stepHandler(settings: Settings) (proc: IProcessContainer) =
        _instructionCounter <- _instructionCounter + 1
        if _instructionCounter  >= settings.NumberOfInstructionToEmulate then
            _sandbox.Stop()

        // invoke services
        _metrics.EmulatedInstruction(proc, _instructionCounter)
        _dumper.Step(proc.ProgramCounter.Value |> BitVector.toUInt32)
        _debugger.Process()

    let private getFileContent(settings: Settings) =
        if settings.DecodeContent then
            let buffer = 
                Convert.FromBase64String(File.ReadAllText(settings.Filename))
                |> Array.map(fun b -> b ^^^ 0xAAuy)
            
            buffer.[0] <- byte 'M'
            buffer.[1] <- byte 'Z'
            
            buffer
        else
            File.ReadAllBytes(settings.Filename)

    let private initialize(settings: Settings) =
        // setup handlers
        let proc = _sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(_dumper.MemoryAccessedHandler proc)
        proc.Step.Add(stepHandler settings)

        // add this file as library for method hooking
        _sandbox.AddLibrary(typeof<Dumper>.Assembly)

        // add all the specified input libraries
        settings.Libs 
        |> Array.iter(fun lib -> 
            _logger?LoadLibrary(lib)
            _sandbox.AddLibrary(lib)
        )

        // configure debugger
        _debugger.PrintDisassembly <- settings.PrintDisassembly
        _debugger.PrintIR <- settings.PrintIR

    let private runSample(settings: Settings) =
        try
            _logger?Start()
            _logger?Details(settings.Filename, _sandbox.GetRunningProcess().Pid)

            // run debugger
            _debugger.Start()

            // run the sample till the end or exception
            _sandbox.Run()
            _logger?Completed()
            true
        with e ->
            // Exception due to some limitation in this emulator
            _logger?Exception(_sandbox.GetRunningProcess().ProgramCounter.Value, e)
            false        
                    
    let private configureLogging(logLevel: LogLevel) =
        let logProvider = new LogProvider()
        logProvider.AddLogger(new ConsoleLogger(logLevel))
        let logFile = Path.Combine(Utility.getResultDir(_sandbox.GetRunningProcess().Pid), "output.log")
        logProvider.AddLogger(new FileLogger(logLevel, logFile))
        logProvider.AddLogSourceToLoggers(_logger)

        // service loggers
        _dumper.ConfigureLogger(logProvider)
        _metrics.ConfigureLogger(logProvider)

    let private saveSnapshot(settings: Settings) =
        if settings.SaveSnapshotOnExit then
            let snapshotManager = new SnapshotManager(_sandbox)
            snapshotManager.TakeSnaphot().SaveTo(settings.SnapshotToSave)
            _logger?SnapshotSaved(settings.SnapshotToSave)

    let private loadSnapshot(settings: Settings) =
        if settings.LoadSnapshotOnStart then
            if File.Exists(settings.SnapshotToLoad) then
                let snapshotManager = new SnapshotManager(_sandbox)
                let snapshot = Model.Snapshot.Read(settings.SnapshotToLoad)
                snapshotManager.LoadSnapshot(snapshot)
                _logger?SnapshotLoaded(settings.SnapshotToLoad)
            else
                _logger?SnapshotNotFound(settings.SnapshotToLoad)

    [<EntryPoint>]
    let main argv = 
        Cli.printBanner()        
        match getSettings(argv) with
        | Some settings->     
            _sandbox.Load(getFileContent(settings))

            configureLogging(LogLevel.Informational)
            initialize(settings)
            loadSnapshot(settings)
            if runSample(settings) then
                // the emulation ended correctly
                _dumper.SaveInformation()
                _metrics.SaveInformation()
                saveSnapshot(settings)
            _logger?EmulatedInstructions(_instructionCounter)
            0
        | None -> 
            1