namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2.BinFile
open B2R2.FrontEnd
open B2R2
open B2R2.BinIR
open B2R2.BinIR.LowUIR

module Win32Sandbox =
    type SandboxSettings = {
        PrintAssembly: Boolean
        PrintIR: Boolean
    }

    let defaultSandboxConfig = {
        PrintAssembly = false
        PrintIR = false
    }

    type Win32Sandbox(settings: SandboxSettings) =
        let prinAssembly(handler: BinHandler, instruction: Instruction) =
            if settings.PrintAssembly then
                let disassembledInstruction = BinHandler.DisasmInstr handler false true instruction 
                let instructionBytes = BinHandler.ReadBytes(handler , instruction.Address, int32 instruction.Length)                
                let hexBytes = BitConverter.ToString(instructionBytes).Replace("-"," ")
                let text = String.Format("0x{0,-10} {1, -30} {2}", instruction.Address.ToString("X") + ":", hexBytes, disassembledInstruction)
                Console.WriteLine(text)

        let printIR(statement: Stmt) =
            if settings.PrintIR then
                let statementString = LowUIR.Pp.stmtToString statement
                Console.WriteLine(statementString)

        let runProcess(win32Process: Win32ProcessContainer) =
            let activeRegion = win32Process.GetActiveMemoryRegion()
            let endAddress = activeRegion.BaseAddress + uint64 activeRegion.Size
            let mutable completed = false

            while not completed do
                let instruction = win32Process.GetInstruction()
                completed <- instruction.Address + uint64 instruction.Length >= endAddress

                let handler = win32Process.GetActiveMemoryRegion().Handler
                prinAssembly(handler, instruction)
                                
                // emulate instruction
                BinHandler.LiftInstr handler instruction
                |> Array.iter(fun statement ->
                    printIR(statement)
                    LowUIREmulator.emulateStmt win32Process statement
                )

        // run with default settings
        new() = new Win32Sandbox(defaultSandboxConfig)
        
        member this.Run(filename: String) =
            let win32Process = new Win32ProcessContainer()
            win32Process.Initialize(filename)
            runProcess(win32Process)

        member this.Run(buffer: Byte array) =
            let win32Process = new Win32ProcessContainer()
            win32Process.Initialize(buffer)
            runProcess(win32Process)