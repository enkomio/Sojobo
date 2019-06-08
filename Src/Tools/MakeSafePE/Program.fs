namespace MakeSafePE

open System
open System.Reflection
open System.IO
open System.Reflection.PortableExecutable
open Argu

module Program =

    type CLIArguments =
        | [<MainCommand; Last>] File of file:String     
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | File _ -> "the PE file to translate to safe format."

    let private printColor(msg: String, color: ConsoleColor) =
        Console.ForegroundColor <- color
        Console.WriteLine(msg)
        Console.ResetColor() 

    let private printError(errorMsg: String) =
        printColor(errorMsg, ConsoleColor.Red)

    let private printBanner() =
        Console.ForegroundColor <- ConsoleColor.Cyan        
        let banner = "-=[ Make PE safe ]=-"
        let description = "Transform a PE file in a safe represantion that can be analyzed by Sojobo"

        let year = if DateTime.Now.Year = 2017 then "2017" else String.Format("2017-{0}", DateTime.Now.Year)
        let copy = String.Format("Copyright (c) {0} Enkomio {1}", year, Environment.NewLine)
        Console.WriteLine(banner)
        Console.WriteLine(description)
        Console.WriteLine(copy)
        Console.ResetColor()

    let private printUsage(body: String) =
        Console.WriteLine(body)

    let private encodeContant(buffer: Byte array) =
        buffer
        |> Array.mapi(fun i b ->
            (^^^) 
                (
                    // make header invalid after decryption
                    if i = 0 || i = 1 
                    then 0uy
                    else b
                ) (0xAAuy)
        )
        |> Convert.ToBase64String

    let private makeFileSafe(filename: String) =
        let destFile = Path.GetFileName(filename) + ".txt"

        try
            // try to create a pe object in order to be sure that is a valid PE file
            let fileContent = File.ReadAllBytes(filename)
            use memStream = new MemoryStream(fileContent)
            use pe = new PEReader(memStream)
            let encodedContent = encodeContant(fileContent)
            File.WriteAllText(destFile, encodedContent)
            Console.WriteLine("Encoded content written to: {0}", destFile)
            pe.Dispose()            
            0
        with _ -> 
            Console.WriteLine("File {0}, doesn't seem to be a valid PE file", filename)
            1

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
                match results.TryGetResult(<@ File @>) with
                | Some filename -> 
                    if File.Exists(filename) then
                        makeFileSafe(filename)
                    else
                        printError(String.Format("File {0} doesn't exists", filename))
                        1
                | None ->
                    printUsage(parser.PrintUsage())  
                    0
        with 
            | :? ArguParseException ->
                printUsage(parser.PrintUsage())   
                1
            | e ->
                printError(e.ToString())
                1
