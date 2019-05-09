namespace ES.Sojobo

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open B2R2.FrontEnd
open B2R2
open ES.Sojobo.Model
open B2R2.BinIR

type Win32Sandbox() as this =
    inherit BaseSandbox()

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
        let block = 
            BinHandler.LiftInstr handler instruction
            |> BinHandler.Optimize
        LowUIREmulator.emulateBlock this block

    let emulateBufferInstruction(baseProcess: BaseProcessContainer, buffer: Byte array) =
        // compose instruction
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue        

        // emulate instruction
        let handler = baseProcess.GetActiveMemoryRegion().Handler
        emulateInstruction(handler, instruction, baseProcess)

    let executeStackFrameSetup(baseProcess: BaseProcessContainer) =
        emulateBufferInstruction(baseProcess, [|0x8Buy; 0xFFuy|]) // mov edi, edi
        emulateBufferInstruction(baseProcess, [|0x55uy|]) // push ebp
        emulateBufferInstruction(baseProcess, [|0x8Buy; 0xECuy|]) // mov ebp, esp

    let executeStackFrameCleanup(baseProcess: BaseProcessContainer) =
        emulateBufferInstruction(baseProcess, [|0x8Buy; 0xE5uy|]) // mov esp, ebp
        emulateBufferInstruction(baseProcess, [|0x5Duy|]) // pop ebp

    let executeReturn(baseProcess: BaseProcessContainer, mi: MethodInfo, callbackResult: CallbackResult) =
        let bytesToPop = (mi.GetParameters().Length - 1) * 4 
        
        // compose buffer
        use memWriter = new MemoryStream()
        use binWriter = new BinaryWriter(memWriter)

        if bytesToPop > 0 && callbackResult.Convention = CallingConvention.Cdecl then            
            binWriter.Write(0xC2uy)
            binWriter.Write(bytesToPop)
            memWriter.SetLength(int64 3)
        else            
            binWriter.Write(0xC3uy)        
            memWriter.SetLength(int64 1)
        
        // compose instruction
        binWriter.Flush()
        let arrayBuffer = memWriter.ToArray()
        emulateBufferInstruction(baseProcess, arrayBuffer)

    let resolveLibraryFunctions(assemblies: Assembly seq) =
        assemblies
        |> Seq.collect(fun assembly -> assembly.GetTypes())
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.IsStatic && m.ReturnType = typeof<CallbackResult>)
        |> Seq.filter(fun m -> m.GetParameters().Length > 0 && m.GetParameters().[0].ParameterType = typeof<ISandbox>)
        |> Seq.filter(fun m ->
            // The first parameter must be a IProcessContainer
            // All the subsequence parameters (if nay) must be 
            // integer, which value is directly takenfrom the stack
            let parameters = m.GetParameters()
            if parameters.Length > 1 then
                parameters
                |> Array.skip 1
                |> Array.forall(fun p -> 
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

    let getArguments(baseProcess: BaseProcessContainer, mi: MethodInfo) =
        mi.GetParameters()
        |> Array.skip 1
        |> Array.mapi(fun i p ->
            let argi = baseProcess.GetArgument(i)
            if p.ParameterType = typeof<UInt32>
            then BitVector.toUInt32 argi.Value :> Object
            else BitVector.toInt32 argi.Value :> Object
        )

    let setResult(baseProcess: BaseProcessContainer, callbackResult: CallbackResult) =
        callbackResult.ReturnValue
        |> Option.iter(fun retValue ->
            let eax = createVariableWithValue("EAX", EmulatedType.DoubleWord, retValue)
            baseProcess.SetVariable(eax)
        )

    let invokeLibraryFunction(sandbox: ISandbox, baseProcess: BaseProcessContainer) =
        let keyName = _callbacks.[baseProcess.GetProgramCounter().Value |> BitVector.toUInt64]
        let libraryFunction = _libraryFunctions.[keyName]

        executeStackFrameSetup(baseProcess)
        let arguments = Array.concat [
            [|sandbox :> Object|]
            getArguments(baseProcess, libraryFunction)
        ]

        let libraryFunctionResult = libraryFunction.Invoke(null, arguments) :?> CallbackResult
        setResult(baseProcess, libraryFunctionResult)
        executeStackFrameCleanup(baseProcess)
        executeReturn(baseProcess, libraryFunction, libraryFunctionResult)

    member this.AddLibrary(assembly: Assembly) =
        _assemblies.Add(assembly)

    default this.Run() =            
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
            let programCounter = win32Process.GetProgramCounter().Value |> BitVector.toUInt64
            if programCounter |> _callbacks.ContainsKey then
                invokeLibraryFunction(this, win32Process)
            else                    
                let instruction = win32Process.ReadNextInstruction()
                _stopExecution <- _stopExecution || programCounter >= endAddress

                // emulate instruction
                let handler = win32Process.GetActiveMemoryRegion().Handler                    
                emulateInstruction(handler, instruction, win32Process)

    default this.Stop() =
        _stopExecution <- true
        
    default this.Load(filename: String) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(filename)

    default this.Load(buffer: Byte array) =
        _currentProcess <- new Win32ProcessContainer() |> Some
        _currentProcess.Value.Initialize(buffer)

    default this.GetRunningProcess() =
        _currentProcess.Value :> IProcessContainer