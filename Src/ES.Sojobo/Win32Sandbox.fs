namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2.BinFile
open B2R2.FrontEnd
open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR

type SandboxSettings = {
    PrintAssembly: Boolean
    PrintIR: Boolean
}

type Win32Sandbox(settings: SandboxSettings) =
    let prinAssembly(win32Process: Win32ProcessContainer, instruction: Instruction) =
        if settings.PrintAssembly then
            let handler = win32Process.GetProcessHandler()
            let disassembledInstruction = BinHandler.DisasmInstr handler true true instruction 
            Console.WriteLine(disassembledInstruction)

    let printIR(win32Process: Win32ProcessContainer, statements: Stmt array) =
        statements
        |> Array.iter(fun statement ->
            let statementString = LowUIR.Pp.stmtToString statement
            Console.WriteLine(statementString)
        )

    let runSection(win32Process: Win32ProcessContainer) (section: Section) =  
        let handler = win32Process.GetProcessHandler()
        let sectionRange = section.ToAddrRange()
        let mutable progCounter = handler.FileInfo.EntryPoint
        let endAddress = AddrRange.GetMax(sectionRange)

        while progCounter < endAddress do
            match BinHandler.LiftIRBBlock handler progCounter with
            | Ok (instrIR, newxtAddress) -> 
                instrIR
                |> List.iter(fun (instruction, statements) ->
                    prinAssembly(win32Process, instruction)
                    printIR(win32Process, statements)
                    LowUIREmulator.emulateStmts win32Process statements
                )
            | Error statements -> 
                ()

    let runProcess(win32Process: Win32ProcessContainer) =
        win32Process.GetProcessHandler().FileInfo.GetExecutableSections()
        |> Seq.iter(runSection win32Process)

    // run with default settings
    new() = new Win32Sandbox({
        PrintAssembly = false
        PrintIR = false
    })
        
    member this.Run(filename: String) =
        let win32Process = new Win32ProcessContainer()
        win32Process.Initialize(filename)
        runProcess(win32Process)

    member this.Run(buffer: Byte array) =
        let win32Process = new Win32ProcessContainer()
        win32Process.Initialize(buffer)
        runProcess(win32Process)