namespace ES.Sojobo

open System
open System.Reflection
open System.Collections.Generic
open B2R2.FrontEnd
open B2R2
open ES.Sojobo.Model

type Win32Sandbox() =
    let _assemblies = new List<Assembly>()
    let _libraryFunctions = new Dictionary<String, MethodInfo>()
    let _callbacks = new Dictionary<UInt64, String>()       
    let mutable _stopExecution = false
    let mutable _currentProcess: Win32ProcessContainer option = None

    let getFunctionKeyName(functioName: String, libraryName: String) =
        let keyName = String.Format("{0}::{1}", libraryName, functioName).ToLower()
        keyName.Replace(".dll", String.Empty)

    let mapImportedFunctions(win32Process: Win32ProcessContainer) =
        win32Process.GetImportedFunctions()
        |> Seq.iter(fun symbol ->
            let keyName = getFunctionKeyName(symbol.Name, symbol.LibraryName)
            if _libraryFunctions.ContainsKey(keyName) then
                _callbacks.[symbol.Address] <- keyName
                let addressBytes = uint32 symbol.Address |> BitConverter.GetBytes
                win32Process.WriteMemory(symbol.Address, addressBytes)
        )

    let emulateInstruction(handler: BinHandler, instruction: Instruction, baseProcess: BaseProcessContainer) =
        BinHandler.LiftInstr handler instruction
        |> Array.iter(LowUIREmulator.emulateStmt baseProcess)

    let executeReturn =
        let arrayBuffer = Array.zeroCreate<Byte>(1)
        arrayBuffer.[0] <- 0xC3uy
        (*
        // TODO:
        if I have to clean the stack is: C2 0400                  | ret 4 
        *)

        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, arrayBuffer)
        let retInstruction = BinHandler.ParseInstr handler Addr.MinValue
        fun (baseProcess: BaseProcessContainer) -> 
            let handler = baseProcess.GetActiveMemoryRegion().Handler
            emulateInstruction(handler, retInstruction, baseProcess)

    let resolveLibraryFunctions(assemblies: Assembly seq) =
        assemblies
        |> Seq.collect(fun assembly -> assembly.GetTypes())
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.IsStatic && m.ReturnType = typeof<CallbackResult>)
        |> Seq.filter(fun m -> m.GetParameters().Length > 0 && m.GetParameters().[0].ParameterType = typeof<IProcessContainer>)
        |> Seq.filter(fun m ->
            let parameters = m.GetParameters()
            if parameters.Length > 1 then
                parameters
                |> Array.skip 1
                |> Array.forall(fun p -> 
                    // must be all integers
                    [typeof<Int32>; typeof<UInt32>]
                    |> List.contains(p.ParameterType)
                )
            else
                true
        )
        |> Seq.iter(fun m ->
            let keyName = getFunctionKeyName(m.Name, m.DeclaringType.Name)
            _libraryFunctions.[keyName] <- m
        )

    let invokeLibraryFunction(baseProcess: BaseProcessContainer) =
        let keyName = _callbacks.[baseProcess.GetProgramCounterValue()]
        let libraryFunction = _libraryFunctions.[keyName]
        let libraryFunctionResult = libraryFunction.Invoke(null, [|baseProcess|])
        executeReturn(baseProcess)

    member this.AddLibrary(assembly: Assembly) =
        _assemblies.Add(assembly)

    member this.Run() =            
        let win32Process = _currentProcess.Value
        let activeRegion = win32Process.GetActiveMemoryRegion()
        let endAddress = activeRegion.BaseAddress + uint64 activeRegion.Content.Length
        
        // prepare for execution
        resolveLibraryFunctions([Assembly.GetExecutingAssembly()])
        resolveLibraryFunctions(_assemblies)
        mapImportedFunctions(win32Process)
                        
        // start execution loop
        _stopExecution <- false
        while not _stopExecution do
            // check if called an emulated function
            if win32Process.GetProgramCounterValue() |> _callbacks.ContainsKey then
                invokeLibraryFunction(win32Process)
            else                    
                let instruction = win32Process.ReadNextInstruction()
                _stopExecution <- _stopExecution || instruction.Address + uint64 instruction.Length >= endAddress

                // emulate instruction
                let handler = win32Process.GetActiveMemoryRegion().Handler                    
                emulateInstruction(handler, instruction, win32Process)

    member this.Stop() =
        _stopExecution <- true
        
    member this.Create(filename: String) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(filename)

    member this.Create(buffer: Byte array) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(buffer)

    member this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer

    interface  ISandbox with
        member this.Create(filename: String) =
            this.Create(filename)

        member this.Create(buffer: Byte array) =
            this.Create(buffer)

        member this.Run() =
            this.Run()

        member this.Stop() =
            this.Stop()
            
        member this.GetRunningProcess() =
            this.GetRunningProcess()