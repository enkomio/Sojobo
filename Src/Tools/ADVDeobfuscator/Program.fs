namespace ES.ADVDeobfuscator

open System
open System.IO
open System.Reflection
open ES.Fslog
open ES.Fslog.Loggers

module Program =

    let configureLogging(isVerbose: Boolean) =        
        let logLevel = if isVerbose then LogLevel.Verbose else LogLevel.Informational
        let logProvider = new LogProvider()
        logProvider.AddLogger(new ConsoleLogger(logLevel))
        let logFile = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "adv-deobfuscator.log")
        logProvider.AddLogger(new FileLogger(logLevel, Path.GetFullPath(logFile)))
        logProvider

    [<EntryPoint>]
    let main argv =
        if argv.Length < 1 then
            Console.WriteLine("Please specify the file to analyze (also --verbose for verbose log messages)")
            1
        else
            let fileName = argv |> Array.last
            let isVerbose = argv |> Array.contains("--verbose")
            let logProvider = configureLogging(isVerbose)

            let logger =
                log "ADVDeobfuscator"
                |> info "File" "Deobfuscate file: {0}"
                |> buildAndAdd(logProvider)

            logger?File(fileName)
            let deobfuscator = new Deobfuscator(fileName, logProvider)
            deobfuscator.Deobfuscate()
            0
