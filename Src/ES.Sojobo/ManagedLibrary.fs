namespace ES.Sojobo

open System
open System.Reflection
open System.IO
open System.Collections.Generic
open B2R2
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2.BinFile

type ManagedLibrary(assembly: Assembly, emulator: IEmulator, pointerSize: Int32) =
    let pointerSizeInBytes = pointerSize / 8

    let getArgument(proc: IProcessContainer, position: Int32) =
        let ebp = proc.Cpu.GetRegister("EBP").Value |> BitVector.toUInt32
        let address = ebp + uint32 (position + 2) * 4ul
        let buffer = proc.Memory.ReadMemory(uint64 address, sizeof<UInt32>)
        let varName = Helpers.getTempName(string position, EmulatedType.DoubleWord)            
        {createVariable(varName, EmulatedType.DoubleWord) with Value = BitVector.ofArr(buffer)}

    let getArguments(proc: IProcessContainer, mi: MethodInfo) =
        mi.GetParameters()
        |> Array.skip 1
        |> Array.mapi(fun i p ->
            let argi = getArgument(proc, i)
            if p.ParameterType = typeof<UInt32>
            then BitVector.toUInt32 argi.Value :> Object
            else BitVector.toInt32 argi.Value :> Object
        )

    let setResult(proc: IProcessContainer, callbackResult: CallbackResult) =
        callbackResult.ReturnValue
        |> Option.iter(fun retValue ->
            let eax = createVariableWithValue("EAX", EmulatedType.DoubleWord, retValue)
            proc.Cpu.SetRegister(eax)
        )

    let emulateInstruction(proc: IProcessContainer, instruction: Instruction) =
        let handler = proc.GetActiveMemoryRegion().Handler
        emulator.EmulateInstruction(handler, instruction)

    let emulateMovEdiEdi =
        let buffer = [|0x8Buy; 0xFFuy|]
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulatePushEbp =
        let buffer = [|0x55uy|]
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulateMovEbpEsp =
        let buffer = [|0x8Buy; 0xECuy|]
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulateMovESpEbp =
        let buffer = [|0x8Buy; 0xE5uy|]
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulatePopEbp =
        let buffer = [|0x5Duy|]
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let executeStackFrameSetup(proc: IProcessContainer) =
        emulateMovEdiEdi(proc)
        emulatePushEbp(proc)
        emulateMovEbpEsp(proc)

    let executeStackFrameCleanup(proc: IProcessContainer) =
        emulateMovESpEbp(proc)
        emulatePopEbp(proc)

    let executeReturn(proc: IProcessContainer, mi: MethodInfo, callbackResult: CallbackResult) =
        let bytesToPop = (mi.GetParameters().Length - 1) * pointerSizeInBytes
        
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
        let handler = BinHandler.Init(ISA.OfString "x86", ArchOperationMode.NoMode, true, Addr.MinValue, memWriter.ToArray())
        let instruction = BinHandler.ParseInstr handler Addr.MinValue        

        // emulate instruction
        let handler = proc.GetActiveMemoryRegion().Handler
        emulator.Emulate(handler, instruction)

    let getOrCreateIatRegion(memoryManager: MemoryManager, symbols: BinFile.Symbol seq) =
        memoryManager.GetMemoryMap()
        |> Array.tryFind(fun region ->
            region.Info.Equals("IAT_" + assembly.GetName().Name)
        )
        |> function
            | Some region -> region
            | None ->
                let size = (symbols |> Seq.length) * pointerSizeInBytes
                let baseAddress = memoryManager.GetFreeMemory(size)
                let newRegion = 
                    {createMemoryRegion(baseAddress, size, Permission.Readable) with
                        Info = "IAT_" + assembly.GetName().Name
                    }
                memoryManager.AddMemoryRegion(newRegion)
                newRegion

    member val EmulatedMethods = new Dictionary<String, MethodInfo>() with get
    member val Callbacks = new Dictionary<UInt64, String>() with get, set    
    
    override this.ToString() =
        String.Format("Name: {0}, #Callbacks {1}", assembly.FullName, this.Callbacks.Count)

    member private this.MapEmulatedMethods(exportedMethods: IDictionary<String, UInt64>) =
        this.EmulatedMethods
        |> Seq.iteri(fun index kv ->
            match exportedMethods.TryGetValue(kv.Key) with
            | (true, address) -> this.Callbacks.[address] <- kv.Key
            | _ -> ()
        )

    member private this.MapImportAddressTableMethods
        (
            memoryManager: MemoryManager, 
            importedSymbols: BinFile.Symbol seq, 
            iatRegion: MemoryRegion,
            exportedMethods: IDictionary<String, UInt64>
        ) =
        importedSymbols
        |> Seq.iteri(fun index symbol ->
            let keyName = Helpers.getFunctionKeyName(symbol.Name, symbol.LibraryName)
            let offset = 
                match exportedMethods.TryGetValue(keyName) with
                | (true, address) -> address
                | _ -> iatRegion.BaseAddress + uint64 (index * (pointerSize / 8))
           
            // map function
            if this.EmulatedMethods.ContainsKey(keyName) then                
                this.Callbacks.[offset] <- keyName

                // write the function address to IAT
                let addressBytes = uint32 offset |> BitConverter.GetBytes
                memoryManager.WriteMemory(symbol.Address, addressBytes, false)
        )

    member internal this.GetAssembly() =
        assembly

    member this.GetAddress(name: String) =
        this.Callbacks
        |> Seq.find(fun kv -> kv.Value.Equals(name, StringComparison.OrdinalIgnoreCase))
        |> fun kv -> kv.Key

    member internal this.MapSymbolWithManagedMethods(memoryManager: MemoryManager, symbols: BinFile.Symbol seq, exportedMethods: IDictionary<String, UInt64>) =
        if this.EmulatedMethods.Count > 0 && (symbols |> Seq.length) > 0 then
            let iatRegion = getOrCreateIatRegion(memoryManager, symbols)
            this.MapImportAddressTableMethods(memoryManager, symbols, iatRegion, exportedMethods)
            this.MapEmulatedMethods(exportedMethods)

    member internal this.ResolveLibraryFunctions() =
        assembly.GetTypes()
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.IsStatic && m.ReturnType = typeof<CallbackResult>)
        |> Seq.filter(fun m -> m.GetParameters().Length > 0 && m.GetParameters().[0].ParameterType = typeof<ISandbox>)
        |> Seq.filter(fun m ->
            // The first parameter must be a IProcessContainer
            // All the subsequence parameters (if any) must be 
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
            let keyName = Helpers.getFunctionKeyName(m.Name, m.DeclaringType.Name)
            this.EmulatedMethods.[keyName] <- m
        )

    member internal this.IsLibraryCall(programCounter: UInt64) =
        programCounter |> this.Callbacks.ContainsKey

    member internal this.InvokeLibraryFunction(sandbox: ISandbox) =
        let proc = sandbox.GetRunningProcess()
        let keyName = this.Callbacks.[proc.ProgramCounter.Value |> BitVector.toUInt64]
        let libraryFunction = this.EmulatedMethods.[keyName]

        executeStackFrameSetup(proc)
        let arguments = Array.concat [
            [|sandbox :> Object|]
            getArguments(proc, libraryFunction)
        ]

        let libraryFunctionResult = libraryFunction.Invoke(null, arguments) :?> CallbackResult
        setResult(proc, libraryFunctionResult)
        executeStackFrameCleanup(proc)
        executeReturn(proc, libraryFunction, libraryFunctionResult)

    member internal this.Initialize(sandbox: ISandbox) =
        assembly.GetTypes()
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.Name.Equals("Initialize", StringComparison.Ordinal))
        |> Seq.filter(fun m -> m.IsStatic && m.ReturnType = typeof<Void>)
        |> Seq.filter(fun m -> m.GetParameters().Length = 1 && m.GetParameters().[0].ParameterType = typeof<ISandbox>)
        |> Seq.iter(fun m -> m.Invoke(null, [|sandbox|]) |> ignore)