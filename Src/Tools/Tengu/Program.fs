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
        |> info "Completed" "-=[ Analysis Completed ]=-"
        |> info "MemoryDumped" "Dynamic code dumped to: {0}"
        |> build

    let writeDisassembly(proc: IProcessContainer) =
        let text = ES.Sojobo.Utility.formatCurrentInstruction(proc)
        Console.WriteLine(text)

    let writeIR(proc: IProcessContainer) =
        ES.Sojobo.Utility.formatCurrentInstructionIR(proc)
        |> Array.iter(Console.WriteLine)

    let stepHandler(settings: Settings) (proc: IProcessContainer) =
        _instructionCounter <- _instructionCounter + 1
        if settings.NumberOfInstructionToEmulate >= _instructionCounter then
            _sandbox.Stop()

        if settings.PrintDisassembly then writeDisassembly(proc)
        if settings.PrintIR then writeIR(proc)

        _metrics.EmulatedInstruction(proc.GetInstruction(), _instructionCounter)
        _dumper.Step(proc.ProgramCounter.Value |> BitVector.toUInt32)

    let initialize(settings: Settings) =
        _sandbox.Load(File.ReadAllBytes(settings.Filename))

        // setup handlers
        let proc = _sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(_dumper.MemoryAccessedHandler proc)
        proc.Step.Add(stepHandler settings)

        // add this file as library for method hooking
        _sandbox.AddLibrary(typeof<Dumper>.Assembly)

    let runSample() =
        try
            _logger?Start()
            // run the sample till the end or exception
            _sandbox.Run()
        with _ ->
            // Exception due to some limitation in this emulator
            ()

        _logger?Completed()

    let getResultDir() =
        let curDir = Path.GetFileName(Assembly.GetEntryAssembly().Location)
        Path.Combine(curDir, _sandbox.GetRunningProcess().Pid.ToString())

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
        sb.AppendLine("Instruction counter, Stack frame counter") |> ignore
        _metrics.GetMetrics()
        |> Array.iter(fun metric -> 
            sb
                .AppendFormat("{0}, {1}", metric.InstructionCounter, metric.StackFrameCounter)
                .AppendLine() 
            |> ignore
        )
        File.WriteAllText(Path.Combine(getResultDir(), "metrics_stack_frame.txt"), sb.ToString())

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
            runSample()
            collectInformation()
            0
        | None -> 
            1