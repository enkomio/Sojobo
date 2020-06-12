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
        let fileName = argv.[0]
        let logProvider = configureLogging(false)

        let logger =
            log "ADVDeobfuscator"
            |> info "File" "Deobfuscate file: {0}"
            |> buildAndAdd(logProvider)

        logger?File(fileName)
        let deobfuscator = new Deobfuscator(fileName, logProvider)
        deobfuscator.Deobfuscate()
        0
