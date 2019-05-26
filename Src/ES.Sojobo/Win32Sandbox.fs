namespace ES.Sojobo

open System
open System.Reflection
open System.IO
open B2R2.FrontEnd
open B2R2
open B2R2.FrontEnd.Intel
open ES.Sojobo.Win32
open ES.Sojobo.Model

type Win32SandboxSettings = {
    /// This settings will initialize the environment by loading
    /// missing libraries or setup default hook for emulated functions
    InitializeEnvironment: Boolean
} with
    static member Default = {
        InitializeEnvironment = true
    }

type Win32Sandbox(settings: Win32SandboxSettings) as this =
    inherit BaseSandbox()
     
    let mutable _stopExecution = false
    let mutable _currentProcess: Win32ProcessContainer option = None    

    static let notRegisteredFunction(sandbox: ISandbox) =
        let programCounter = sandbox.GetRunningProcess().GetProgramCounter().Value |> BitVector.toUInt32
        let keyName = (sandbox :?> BaseSandbox).Callbacks.[uint64 programCounter]
        raise (UnhandledFunction keyName) |> ignore

    let mapSymbolWithManagedFunctions(memoryManager: MemoryManager, symbols: BinFile.Symbol seq) =
        let regionBaseAddress = memoryManager.AllocateMemory((symbols |> Seq.length) * 4, MemoryProtection.Read)
        let region = memoryManager.GetMemoryRegion(regionBaseAddress)
        
        symbols
        |> Seq.iteri(fun index symbol ->
            // obtains function offset
            let keyName = Utility.getFunctionKeyName(symbol.Name, symbol.LibraryName)
            let offset = region.BaseAddress + uint64 (index * 4)
            this.Callbacks.[offset] <- keyName

            // write the function address
            let addressBytes = uint32 offset |> BitConverter.GetBytes
            memoryManager.UnsafeWriteMemory(symbol.Address, addressBytes, false)  

            // map unhandled function if necessary
            if this.LibraryFunctions.ContainsKey(keyName) |> not then                
                let methodInfo = this.GetType().GetMethod("notRegisteredFunction", BindingFlags.NonPublic ||| BindingFlags.Static)
                this.LibraryFunctions.[keyName] <- methodInfo
        )

    let emulateInstruction(handler: BinHandler, instruction: Instruction) =
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
        emulateInstruction(handler, instruction)

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
        (*
    let resolveLibraryFunctionsFromAssembly(assembly: Assembly) =
        assembly.GetTypes()
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
            this.LibraryFunctions.[keyName] <- m
        )
        
    let resolveLibraryFunctionsFromContent(content: Byte array, filename: String option) =
        try
            let assembly = Assembly.Load(content)
            resolveLibraryFunctionsFromAssembly(assembly)
        with 
            | :? BadImageFormatException ->
                // map the file
                let isa = ISA.OfString "x86"
                let handler = 
                    match filename with
                    | Some filename -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)
                    | None -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, content)

                // it isn't a .NET file, try to map exported functions
                match handler.FileInfo.FileFormat with
                | FileFormat.PEBinary ->
                    Utility.mapPeHeader(handler, this.GetRunningProcess().Memory)
                    Utility.mapSections(handler, this.GetRunningProcess().Memory)
                
                    // map callback for exported function
                    let libraryName =
                        match filename with
                        | Some filename -> Path.GetFileName(filename)
                        | _ -> String.Empty

                    let pe = Utility.getPe(handler)
                    pe.ExportMap
                    |> Map.map(fun addr funName -> 
                        {
                            Address = addr
                            Name = funName
                            Kind = BinFile.SymbolKind.ExternFunctionType
                            Target = BinFile.TargetKind.StaticSymbol
                            LibraryName = libraryName
                        }: BinFile.Symbol
                    )
                    |> Seq.map(fun kv -> kv.Value)
                    |> fun symbols -> mapSymbolWithManagedFunctions(this.GetRunningProcess().Memory, symbols)
                | _ ->
                    // just map the file in memory
                    this.GetRunningProcess().Memory.AllocateMemory(content, MemoryProtection.Read)
                    |> ignore


    let resolveLibraryFunctionsFromFile(filename: String) =
        if File.Exists(filename) then
            let content = File.ReadAllBytes(filename)
            resolveLibraryFunctionsFromContent(content, Some filename)

    let resolveLibraryFunctions(libraries: Library seq) =
        libraries
        |> Seq.iter(function
            | Assembly assembly -> resolveLibraryFunctionsFromAssembly(assembly)
            | Native content -> resolveLibraryFunctionsFromContent(content, None)
            | File filename -> resolveLibraryFunctionsFromFile(filename)
        )
        *)
    let getArgument(proc: IProcessContainer, position: Int32) =
        let ebp = proc.GetRegister("EBP").Value |> BitVector.toUInt32
        let address = ebp + uint32 (position + 2) * 4ul
        let buffer = proc.Memory.ReadMemory(uint64 address, sizeof<UInt32>)
        let varName = Utility.getTempName(string position, EmulatedType.DoubleWord)        
        {createVariable(varName, EmulatedType.DoubleWord) with Value = BitVector.ofArr(buffer)}

    let getArguments(baseProcess: BaseProcessContainer, mi: MethodInfo) =
        mi.GetParameters()
        |> Array.skip 1
        |> Array.mapi(fun i p ->
            let argi = getArgument(baseProcess, i)
            if p.ParameterType = typeof<UInt32>
            then BitVector.toUInt32 argi.Value :> Object
            else BitVector.toInt32 argi.Value :> Object
        )

    let setResult(baseProcess: BaseProcessContainer, callbackResult: CallbackResult) =
        callbackResult.ReturnValue
        |> Option.iter(fun retValue ->
            let eax = createVariableWithValue("EAX", EmulatedType.DoubleWord, retValue)
            baseProcess.SetRegister(eax)
        )

    let invokeLibraryFunction(sandbox: ISandbox, baseProcess: BaseProcessContainer) =
        let keyName = this.Callbacks.[baseProcess.GetProgramCounter().Value |> BitVector.toUInt64]
        let libraryFunction = this.LibraryFunctions.[keyName]

        executeStackFrameSetup(baseProcess)
        let arguments = Array.concat [
            [|sandbox :> Object|]
            getArguments(baseProcess, libraryFunction)
        ]

        let libraryFunctionResult = libraryFunction.Invoke(null, arguments) :?> CallbackResult
        setResult(baseProcess, libraryFunctionResult)
        executeStackFrameCleanup(baseProcess)
        executeReturn(baseProcess, libraryFunction, libraryFunctionResult)

    let setupTeb() =
        let tebAddress = createTeb(this)
        if this.GetRunningProcess().GetPointerSize() = 32 then [
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 (uint32 tebAddress) 32<rt>)        
        ] else [
            createVariableWithValue(string Register.FSBase, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.QuadWord, BitVector.ofUInt64 tebAddress 64<rt>)        
        ] 
        |> List.iter(this.GetRunningProcess().SetRegister)

    let prepareForExecution() =
        if settings.InitializeEnvironment then
            this.AddLibrary(Assembly.GetExecutingAssembly())
        (*
            resolveLibraryFunctions([Assembly(Assembly.GetExecutingAssembly())])

        resolveLibraryFunctions(this.Libraries)        
        mapSymbolWithManagedFunctions(win32Process.Memory, win32Process.GetImportedFunctions())
        *)
        setupTeb()

    let isProgramCounterMonitore(proc: IProcessContainer) =
        let programCounter = proc.GetProgramCounter().Value |> BitVector.toUInt64
        programCounter |> this.Callbacks.ContainsKey

    let emulateNextInstruction(proc: BaseProcessContainer, endAddress: UInt64) =
        let instruction = proc.ReadNextInstruction()
        let handler = proc.GetActiveMemoryRegion().Handler
        emulateInstruction(handler, instruction)

        // check ending condition
        _stopExecution <- 
            _stopExecution || 
            proc.GetProgramCounter().Value |> BitVector.toUInt64 >= endAddress

    new() = new Win32Sandbox(Win32SandboxSettings.Default)

    default this.Run() =            
        let win32Process = _currentProcess.Value
        let activeRegion = win32Process.GetActiveMemoryRegion()
        let endAddress = activeRegion.BaseAddress + uint64 activeRegion.Content.Length
        
        // initialize structures and hooks    
        prepareForExecution()
                        
        // start execution loop
        _stopExecution <- false
        while not _stopExecution do
            // check if called an emulated function
            if isProgramCounterMonitore(win32Process) then
                invokeLibraryFunction(this, win32Process)
            else                    
                // emulate instruction
                emulateNextInstruction(win32Process, endAddress)

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