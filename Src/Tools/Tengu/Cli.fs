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
        DecodeContent: Boolean
        SaveSnapshotOnExit: Boolean
        Snapshot: String
        Libs: String array
    }

    type CLIArguments =
        | [<MainCommand; Last>] File of file:string   
        | Instruction of count:Int32
        | Print_Disassembly
        | Print_IR
        | Snapshot of name:String
        | Lib of name:String
        | Decode_Content
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | File _ -> "the PE file to analyze."
                | Snapshot _ -> "create a snapshot file when the emulation ends."
                | Print_Disassembly -> "print the disassembly of the emulated instruction."
                | Print_IR -> "print the IR code of the emulated instruction."
                | Lib _ -> "library to include for the emulation."
                | Instruction _ -> "the number of instructions to emulate (default 10000)."
                | Decode_Content -> "decode the content of the file (previously encoded with MakeSafePE)."

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
                        PrintDisassembly = results.Contains(<@ Print_Disassembly @>)
                        PrintIR = results.Contains(<@ Print_IR @>)
                        DecodeContent = results.Contains(<@ Decode_Content @>)
                        Libs = results.GetResults(<@ Lib @>) |> Seq.toArray
                        NumberOfInstructionToEmulate = results.GetResult(<@ Instruction @>, 10000)
                        SaveSnapshotOnExit = results.Contains(<@ Snapshot @>)
                        Snapshot = results.GetResult(<@ Snapshot @>, String.Empty)
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