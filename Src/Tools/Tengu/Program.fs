namespace ES.Tengu

open System
open System.Reflection
open System.IO
open B2R2
open ES.Fslog
open ES.Sojobo
open ES.Tengu.Cli

module Program =
    open ES.Fslog.Loggers
    open System.Text

    let private _sandbox = new Win32Sandbox()
    let private _dumper = new Dumper()
    let private _metrics = new Metrics()
    let mutable private _instructionCounter = 0

    let _logger =
        log "Tangu"
        |> info "Start" "-=[ Start Analysis ]=-"
        |> info "Details" "File: {0} PID: {1}"
        |> info "Completed" "-=[ Analysis Completed ]=-"
        |> info "MemoryDumped" "Dynamic code dumped to: {0}"
        |> info "SavedMetrics" "Saved metrics to: {0}"
        |> error "Exception" "PC: {0} - Error: {1}"
        |> build

    let writeDisassembly(proc: IProcessContainer) =
        let text = ES.Sojobo.Utility.formatCurrentInstruction(proc)
        Console.WriteLine(text)

    let writeIR(proc: IProcessContainer) =
        ES.Sojobo.Utility.formatCurrentInstructionIR(proc)
        |> Array.iter(Console.WriteLine)

    let stepHandler(settings: Settings) (proc: IProcessContainer) =
        _instructionCounter <- _instructionCounter + 1
        if _instructionCounter  >= settings.NumberOfInstructionToEmulate then
            _sandbox.Stop()

        if settings.PrintDisassembly then writeDisassembly(proc)
        if settings.PrintIR then writeIR(proc)

        _metrics.EmulatedInstruction(proc, _instructionCounter)
        _dumper.Step(proc.ProgramCounter.Value |> BitVector.toUInt32)

    let getFileContent(settings: Settings) =
        if settings.DecodeContent then
            let buffer = 
                Convert.FromBase64String(File.ReadAllText(settings.Filename))
                |> Array.map(fun b -> b ^^^ 0xAAuy)
            
            buffer.[0] <- byte 'M'
            buffer.[1] <- byte 'Z'
            
            buffer
        else
            File.ReadAllBytes(settings.Filename)

    let initialize(settings: Settings) =
        _sandbox.Load(getFileContent(settings))

        // setup handlers
        let proc = _sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(_dumper.MemoryAccessedHandler proc)
        proc.Step.Add(stepHandler settings)

        // add this file as library for method hooking
        _sandbox.AddLibrary(typeof<Dumper>.Assembly)

        // add all the specified input libraries
        settings.Libs |> Array.iter(_sandbox.AddLibrary)

    let runSample(settings: Settings) =
        try
            _logger?Start()
            _logger?Details(settings.Filename, _sandbox.GetRunningProcess().Pid)

            // run the sample till the end or exception
            _sandbox.Run()
        with e ->
            // Exception due to some limitation in this emulator
            _logger?Exception(_sandbox.GetRunningProcess().ProgramCounter.Value, e)

        _logger?Completed()

    let getResultDir() =
        let curDir = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let directory = Path.Combine(curDir, "Result", "PID_" + _sandbox.GetRunningProcess().Pid.ToString())
        Directory.CreateDirectory(directory) |> ignore
        directory

    let collectInformation() =
        // save unpacked memory
        _dumper.GetRegions()
        |> Array.iter(fun memRegion ->            
            let filename = String.Format("mem_dump_{0}.bin", memRegion.StartAddress.ToString("X"))
            let file = Path.Combine(getResultDir(), filename)
            File.WriteAllBytes(file, memRegion.Region.Content)
            _logger?MemoryDumped(file)
        )

        // save metricts
        let sb = new StringBuilder()
        sb.AppendLine("Instruction counter, Stack frame counter, Operation type, Function Address") |> ignore
        _metrics.GetMetrics()
        |> Array.iter(fun metric -> 
            sb
                .AppendFormat("{0}, {1}, {2}, {3}", 
                    metric.InstructionCounter, 
                    metric.StackFrameCounter,
                    metric.OperationType,
                    metric.FunctionAddress
                )
                .AppendLine() 
            |> ignore
        )
        let file = Path.Combine(getResultDir(), "metrics_stack_frame.txt")
        File.WriteAllText(file, sb.ToString())
        _logger?SavedMetrics(file)

    let configureLogging(logLevel: LogLevel) =
        let logProvider = new LogProvider()
        logProvider.AddLogger(new ConsoleLogger(logLevel))
        logProvider.AddLogger(new FileLogger(logLevel, Path.Combine(getResultDir(), "output.log")))
        logProvider.AddLogSourceToLoggers(_logger)

    [<EntryPoint>]
    let main argv = 
        Cli.printBanner()        
        match getSettings(argv) with
        | Some settings->            
            initialize(settings)
            configureLogging(LogLevel.Informational)
            runSample(settings)
            collectInformation()
            0
        | None -> 
            1