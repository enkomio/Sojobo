namespace ES.ADVDeobfuscator

open System
open System.IO
open System.Reflection
open Argu
open ES.Fslog
open ES.Fslog.Loggers

module Program =
    type CLIArguments =
        | [<MainCommand; Last>] File of file: String     
        | Instructions of count:Int32
        | Verbose
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | File _ -> "the file to deobfuscate."
                | Instructions _ -> "the number of instruction to emulate before to stop the emulation."
                | Verbose -> "print verbose messages."

    let configureLogging(isVerbose: Boolean) =        
        let logLevel = if isVerbose then LogLevel.Verbose else LogLevel.Informational
        let logProvider = new LogProvider()
        logProvider.AddLogger(new ConsoleLogger(logLevel))
        let logFile = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "adv-deobfuscator.log")
        logProvider.AddLogger(new FileLogger(logLevel, Path.GetFullPath(logFile)))
        logProvider

    let printBanner() =  
        let year = if DateTime.Now.Year = 2020 then "2020" else String.Format("2020-{0}", DateTime.Now.Year)
        let copy = String.Format("Copyright (c) {0} Enkomio {1}", year, Environment.NewLine)
        Console.ForegroundColor <- ConsoleColor.Cyan      
        Console.WriteLine("-=[ ADVDeobfuscator - Deobfuscate ADVObfuscator strings ]=-")
        Console.WriteLine(copy)
        Console.ResetColor()

    let printUsage(body: String) =
        Console.WriteLine(body)

    let printError(errorMsg: String) =
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine(errorMsg)
        Console.ResetColor() 

    [<EntryPoint>]
    let main argv =
        printBanner()

        let parser = ArgumentParser.Create<CLIArguments>()
        try            
            let results = parser.Parse(argv)
                    
            if results.IsUsageRequested then
                printUsage(parser.PrintUsage())
                0
            else
                let isVerbose = results.Contains(<@ Verbose @>)
                let instructionCount = results.GetResult(<@ Instructions @>, 800)
                let fileToDeobfuscate = results.GetResult(<@ File @>)

                let logProvider = configureLogging(isVerbose)
                
                let logger =
                    log "ADVDeobfuscator"
                    |> info "File" "Deobfuscate file: {0}"
                    |> buildAndAdd(logProvider)
                
                logger?File(fileToDeobfuscate)
                let deobfuscator = new Deobfuscator(fileToDeobfuscate, instructionCount, logProvider)
                deobfuscator.Deobfuscate()
                0
        with 
            | :? ArguParseException ->
                printUsage(parser.PrintUsage())   
                1
            | e ->
                printError(e.ToString())
                1