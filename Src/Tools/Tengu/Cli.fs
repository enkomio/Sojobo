namespace ES.Tengu

open System
open System.IO
open Argu

module Cli =
    type Settings = {
        Filename: String
        NumberOfInstructionToEmulate: Int32
        PrintDisassembly: Boolean
        PrintIR: Boolean
    }

    type CLIArguments =
        | [<MainCommand; Last>] File of file:string   
        | InstructionCount of count:Int32
        | PrintDisassembly of Boolean
        | PrintIR of Boolean
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | File _ -> "the PE file to analyze."
                | PrintDisassembly _ -> "print the disassembly of the emulated instruction (default true)."
                | PrintIR _ -> "print the IR code of the emulated instruction (default false)."
                | InstructionCount _ -> "the number of instructions to emulate (default 10000)."

    let private printColor(msg: String, color: ConsoleColor) =
        Console.ForegroundColor <- color
        Console.WriteLine(msg)
        Console.ResetColor() 

    let private printError(errorMsg: String) =
        printColor(errorMsg, ConsoleColor.Red)

    let printBanner() =             
        let banner = "-=[ Tengu binary analyzer ]=-"

        let year = if DateTime.Now.Year = 2017 then "2017" else String.Format("2017-{0}", DateTime.Now.Year)
        let copy = String.Format("Copyright (c) {0} Enkomio {1}", year, Environment.NewLine)

        Console.ForegroundColor <- ConsoleColor.Cyan   
        Console.WriteLine(banner)
        Console.WriteLine(copy)
        Console.ResetColor()

    let private printUsage(body: String) =
        Console.WriteLine(body)

    let getSettings(argv: String array) =
        let parser = ArgumentParser.Create<CLIArguments>()
        try            
            let results = parser.Parse(argv)
                    
            if results.IsUsageRequested then
                printUsage(parser.PrintUsage())
                None
            else
                match results.TryGetResult(<@ File @>) with
                | Some filename when File.Exists(filename) -> Some <| {
                        Filename = filename
                        PrintDisassembly = results.GetResult(<@ PrintDisassembly @>, true)
                        PrintIR = results.GetResult(<@ PrintIR @>, false)
                        NumberOfInstructionToEmulate = results.GetResult(<@ InstructionCount @>, 10000)
                    } 
                | Some filename ->
                    printError(String.Format("File {0} doesn't exists", filename))
                    None
                | _ ->
                    printUsage(parser.PrintUsage())  
                    None
        with 
            | :? ArguParseException ->
                printUsage(parser.PrintUsage())   
                None
            | e ->
                printError(e.ToString())
                None