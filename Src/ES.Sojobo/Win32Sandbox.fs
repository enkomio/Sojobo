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
        let _callbacks = new Dictionary<UInt64, String>()
        let _emulatedCallbacks = new Dictionary<String, Win32ProcessContainer -> unit>()

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

        let getFunctionKeyName(functioName: String, libraryName: String) =
            let keyName = String.Format("{0}::{1}", libraryName, functioName).ToLower()
            keyName.Replace(".dll", String.Empty)

        let mapImportedFunctions(win32Process: Win32ProcessContainer) =
            win32Process.GetImportedFunctions()
            |> Seq.iter(fun symbol ->
                let keyName = getFunctionKeyName(symbol.Name, symbol.LibraryName)
                if _emulatedCallbacks.ContainsKey(keyName) then
                    _callbacks.[symbol.Address] <- keyName
                    let addressBytes = uint32 symbol.Address |> BitConverter.GetBytes
                    win32Process.WriteMemory(symbol.Address, addressBytes)
                Console.WriteLine("[0x{0, -20}] {1} ({2}) from {3}", symbol.Address.ToString("X"), symbol.Name, symbol.Kind, symbol.LibraryName)
            )

        let emulateInstruction(handler: BinHandler, instruction: Instruction, win32Process: Win32ProcessContainer) =
            BinHandler.LiftInstr handler instruction
            |> Array.iter(fun statement ->
                printIR(statement)
                LowUIREmulator.emulateStmt win32Process statement
            )

        let executeReturn =
            let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, FileFormat.RawBinary, Addr.MinValue, [| 0xC3uy|])
            let retInstruction = BinHandler.ParseInstr handler Addr.MinValue
            fun (win32Process: Win32ProcessContainer) -> 
                let handler = win32Process.GetActiveMemoryRegion().Handler
                emulateInstruction(handler, retInstruction, win32Process)

        let runProcess(win32Process: Win32ProcessContainer) =
            mapImportedFunctions(win32Process)            
            let activeRegion = win32Process.GetActiveMemoryRegion()
            let endAddress = activeRegion.BaseAddress + uint64 activeRegion.Content.Length
            let mutable completed = false

            while not completed do
                // check if called an emulated function
                if win32Process.GetProgramCounter() |> _callbacks.ContainsKey then
                    let keyName = _callbacks.[win32Process.GetProgramCounter()]
                    let callback = _emulatedCallbacks.[keyName]
                    callback(win32Process)
                    executeReturn(win32Process)
                else
                    let instruction = win32Process.GetInstruction()
                    completed <- instruction.Address + uint64 instruction.Length >= endAddress

                    let handler = win32Process.GetActiveMemoryRegion().Handler
                    prinAssembly(handler, instruction)
                                
                    // emulate instruction
                    emulateInstruction(handler, instruction, win32Process)

        // run with default settings
        new() = new Win32Sandbox(defaultSandboxConfig)

        member this.AddCallback(functionName: String, moduleName: String, callback: Action<Win32ProcessContainer>) =
            let keyName = getFunctionKeyName(functionName, moduleName)
            _emulatedCallbacks.[keyName] <- FuncConvert.FromAction(callback)
        
        member this.Run(filename: String) =
            let win32Process = new Win32ProcessContainer()
            win32Process.Initialize(filename)
            runProcess(win32Process)

        member this.Run(buffer: Byte array) =
            let win32Process = new Win32ProcessContainer()
            win32Process.Initialize(buffer)
            runProcess(win32Process)