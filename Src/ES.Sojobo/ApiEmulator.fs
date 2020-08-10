namespace ES.Sojobo

open System
open System.Reflection
open System.IO
open System.Collections.Generic
open B2R2
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2.BinFile
open B2R2.FrontEnd.Intel

(**
This Library is used in order to implement the emulation of a given native DLL
*)
type ApiEmulator(assembly: Assembly, emulator: IEmulator) =

    let getArgument(proc: IProcessContainer, position: Int32) =        
        let basePointer = 
            if proc.PointerSize = 32 then proc.Cpu.GetRegister(Register.EBP.ToString())
            else proc.Cpu.GetRegister(Register.RBP.ToString())

        let position = (position + 2) * (proc.PointerSize / 8)
        
        let address = 
            if proc.PointerSize = 32 then uint64(basePointer.As<UInt32>() + uint32 position)
            else basePointer.As<UInt64>() + uint64 position

        let buffer = proc.Memory.ReadMemory(uint64 address, proc.PointerSize / 8)

        let emulatedType =
            if proc.PointerSize = 32 then EmulatedType.DoubleWord
            else EmulatedType.QuadWord

        let varName = Helpers.getTempName(string position, emulatedType)
        {createVariable(varName, emulatedType) with Value = BitVector.ofArr(buffer)}

    let getArguments(proc: IProcessContainer, mi: MethodInfo) =
        mi.GetParameters()
        |> Array.skip 1
        |> Array.mapi(fun i p ->
            let argi = getArgument(proc, i)
            if p.ParameterType = typeof<UInt32> then 
                BitVector.toUInt32 argi.Value :> Object
            elif p.ParameterType = typeof<UInt64> then 
                BitVector.toUInt64 argi.Value :> Object
            elif p.ParameterType = typeof<Int64> then 
                BitVector.toInt64 argi.Value :> Object
            else 
                BitVector.toInt32 argi.Value :> Object
        )

    let setResult(proc: IProcessContainer, callbackResult: CallbackResult) =
        callbackResult.ReturnValue
        |> Option.iter(fun retValue ->
            let resultVar = 
                if proc.PointerSize = 32 then createVariableWithValue(Register.EAX.ToString(), EmulatedType.DoubleWord, retValue)
                else createVariableWithValue(Register.RAX.ToString(), EmulatedType.QuadWord, retValue)
            proc.Cpu.SetRegister(resultVar)
        )

    let emulateInstruction(proc: IProcessContainer, instruction: Instruction) =
        let handler = proc.GetActiveMemoryRegion().Handler
        emulator.EmulateInstruction(handler, instruction) |> ignore

    let emulateMovEdiEdi =
        let buffer = [|0x8Buy; 0xFFuy|]
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulatePushEbp =
        let buffer = [|0x55uy|]
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulateMovEbpEsp =
        let buffer = [|0x8Buy; 0xECuy|]
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulateMovESpEbp =
        let buffer = [|0x8Buy; 0xE5uy|]
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        let instruction = BinHandler.ParseInstr handler Addr.MinValue     
        fun (proc: IProcessContainer) -> emulateInstruction(proc, instruction)

    let emulatePopEbp =
        let buffer = [|0x5Duy|]
        let handler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
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
        let bytesToPop = 
            // for t32 bit all parameters are passed on the stack
            if proc.PointerSize = 32 then (mi.GetParameters().Length - 1) * 4
            // for 64-bit the first 4 parameters are passed via registers
            else (mi.GetParameters().Length - 1 - 4) * 4
        
        // compose buffer
        use memWriter = new MemoryStream()
        use binWriter = new BinaryWriter(memWriter)

        if bytesToPop > 0 && callbackResult.Convention = CallingConvention.Cdecl then            
            binWriter.Write(0xC2uy)
            binWriter.Write(uint16 bytesToPop)
            memWriter.SetLength(int64 3)
        else            
            binWriter.Write(0xC3uy)        
            memWriter.SetLength(int64 1)
        
        let handler = proc.GetActiveMemoryRegion().Handler

        // compose instruction
        binWriter.Flush()
        let handler = BinHandler.Init(handler.ISA, ArchOperationMode.NoMode, false, Addr.MinValue, memWriter.ToArray())
        let instruction = BinHandler.ParseInstr handler Addr.MinValue        

        // emulate instruction        
        emulator.Emulate(handler, instruction) |> ignore

    let getOrCreateEatRegion(proc: IProcessContainer, symbols: BinFile.Symbol seq, pointerSize: Int32) =
        let regionName = "EAT_" + assembly.GetName().Name
        let isa = proc.GetActiveMemoryRegion().Handler.ISA
        proc.Memory.GetMemoryMap()
        |> Array.tryFind(fun region ->
            region.Info.Equals(regionName)
        )
        |> function
            | Some region -> region
            | None ->
                let size = (symbols |> Seq.length) * (pointerSize / 8)
                let baseAddress = proc.Memory.GetFreeMemory(size)
                let newRegion = 
                    {createMemoryRegion(baseAddress, size, Permission.Readable, isa) with
                        Info = regionName
                    }
                proc.Memory.SetMemoryRegion(newRegion)
                newRegion

    member val EmulatedMethods = new Dictionary<String, MethodInfo>() with get
    member val Callbacks = new Dictionary<UInt64, String>() with get, set    
    
    override this.ToString() =
        String.Format("Name: {0}, #Callbacks {1}", assembly.FullName, this.Callbacks.Count)

    member private this.MapEmulatedMethods(exportedMethods: IDictionary<String, UInt64>) =
        this.EmulatedMethods
        |> Seq.iter(fun kv ->
            match exportedMethods.TryGetValue(kv.Key) with
            | (true, address) -> this.Callbacks.[address] <- kv.Key
            | _ -> ()
        )

    member private this.MapExportAddressTableMethods
        (
            memoryManager: MemoryManager, 
            importedSymbols: BinFile.Symbol seq, 
            iatRegion: MemoryRegion,
            exportedMethods: IDictionary<String, UInt64>,
            pointerSize: Int32
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
                memoryManager.WriteMemory(symbol.Address, addressBytes)
        )

    member this.GetAssembly() =
        assembly

    member this.GetAddress(name: String) =
        this.Callbacks
        |> Seq.find(fun kv -> kv.Value.Equals(name, StringComparison.OrdinalIgnoreCase))
        |> fun kv -> kv.Key

    member this.MapSymbolWithManagedMethods(proc: IProcessContainer, exportedMethods: IDictionary<String, UInt64>) =
        let symbols = proc.GetImportedFunctions()
        if this.EmulatedMethods.Count > 0 && (symbols |> Seq.length) > 0 then
            let eatRegion = getOrCreateEatRegion(proc, symbols, proc.PointerSize)
            this.MapExportAddressTableMethods(proc.Memory, symbols, eatRegion, exportedMethods, proc.PointerSize)
            this.MapEmulatedMethods(exportedMethods)

    member this.ResolveLibraryFunctions() =
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

    member this.IsLibraryCall(programCounter: UInt64) =
        programCounter |> this.Callbacks.ContainsKey

    member this.InvokeLibraryFunction(sandbox: ISandbox) =
        let proc = sandbox.GetRunningProcess()
        let keyName = this.Callbacks.[proc.ProgramCounter.Value |> BitVector.toUInt64]
        let libraryFunction = this.EmulatedMethods.[keyName]

        if proc.PointerSize = 32 then
            executeStackFrameSetup(proc)

        let arguments = Array.concat [
            [|sandbox :> Object|]
            getArguments(proc, libraryFunction)
        ]

        let libraryFunctionResult = libraryFunction.Invoke(null, arguments) :?> CallbackResult
        setResult(proc, libraryFunctionResult)
        if proc.PointerSize = 32 then
            executeStackFrameCleanup(proc)
        executeReturn(proc, libraryFunction, libraryFunctionResult)

    member this.Initialize(sandbox: ISandbox) =
        assembly.GetTypes()
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.Name.Equals("Initialize", StringComparison.Ordinal))
        |> Seq.filter(fun m -> m.IsStatic && m.ReturnType = typeof<Void>)
        |> Seq.filter(fun m -> m.GetParameters().Length = 1 && m.GetParameters().[0].ParameterType = typeof<ISandbox>)
        |> Seq.iter(fun m -> m.Invoke(null, [|sandbox|]) |> ignore)