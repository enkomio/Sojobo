namespace ES.Sojobo

open System
open System.Linq
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open B2R2.BinFile.PE
open System.Collections.Generic
open ES.Sojobo.Model
open System.Reflection

module private UnknowLibrary =
    let mutable private _currentIndex = 0

    let getName() =
        _currentIndex <- _currentIndex + 1
        String.Format("Unknown{0}", _currentIndex)

(**
This Object represent a mapped library in the process address space
*)
type NativeLibrary(content: Byte array) =
    let mutable _isLoaded = false
    let mutable _handle: LibraryHandle option = None    

    member val FileName: String option = None with get, set
    member val Name: String option = None with get, set
    member val Content = content with get
    member val EntryPoint = 0UL with get, set
    member val BaseAddress = 0UL with get, set
    member val Exports = new List<Symbol>() with get, set
    member this.Handle
        with get() =
            match _handle with
            | None ->
                _handle <- 
                    {
                        Name = Path.GetFileName(defaultArg this.FileName String.Empty)
                        Value = this.BaseAddress
                    } |> Some
            | _ -> ()
            _handle.Value
    
    static member Create(content: Byte array) =
        new NativeLibrary(content)

    static member Create(fileName: String) =
        let content = File.ReadAllBytes(fileName)
        new NativeLibrary(content, FileName = Some fileName, Name = Some(Path.GetFileName(fileName)))

    override this.ToString() =
        String.Format("{0}:0x{1}", this.GetLibraryName(), this.BaseAddress.ToString("X"))

    member internal this.SetProperties(entryPoint: UInt64, baseAddress: UInt64, exports: List<Symbol>) =
        this.EntryPoint <- entryPoint
        this.BaseAddress <- baseAddress
        this.Exports <- exports
        _isLoaded <- true

    member private this.SetProperties(handler: BinHandler, baseAddress: UInt64) =
        Helpers.getPe(handler)
        |> Option.iter(fun pe ->
            this.SetProperties(
                uint64 pe.PEHeaders.PEHeader.AddressOfEntryPoint, 
                uint64 baseAddress,
                new List<Symbol>(
                    pe.ExportMap
                    |> Seq.map(fun kv -> {                
                            Address = kv.Key
                            Name = kv.Value
                            Kind = SymbolKind.FunctionType
                            Target = TargetKind.DynamicSymbol
                            LibraryName = (defaultArg this.FileName String.Empty) |> Path.GetFileName
                    })
                )
            )
        )
        
    member private this.ApplyRelocation(pe: PE, proc: IProcessContainer, baseAddress: UInt64) =            
        let libraryAddress = baseAddress - pe.PEHeaders.PEHeader.ImageBase

        pe.RelocBlocks
        |> Seq.collect(fun block -> block.Entries |> Seq.map(fun entry -> (block, entry)))
        |> Seq.iter(fun (block, entry) ->
            let address = baseAddress + uint64 block.PageRVA + uint64 entry.Offset                
            let patch = (address + libraryAddress)
            
            match entry.Type with
            | BaseRelocType.ImageRelBasedHighlow ->                 
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint32 patch))
            | BaseRelocType.ImageRelBasedDir64 ->
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(patch))
            | BaseRelocType.ImageRelBasedHigh -> 
                let patch = (patch >>> 16) &&& 0xFFFFUL
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint16 patch))
            | BaseRelocType.ImageRelBasedLow -> 
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint16 patch))
            
            // ignore
            | BaseRelocType.ImageRelBasedHighadj -> ()
            | BaseRelocType.ImageRelBasedMipsJmpaddr
            | BaseRelocType.ImageRelBasedArmMov32
            | BaseRelocType.ImageRelBasedRiscvHigh20 -> ()            
            | BaseRelocType.ImageRelBasedThumbMov32
            | BaseRelocType.ImageRelBasedRiscvLow12I -> ()
            | BaseRelocType.ImageRelBasedRiscvLow12S -> ()
            | BaseRelocType.ImageRelBasedMipsJmpaddr16 -> ()
            | BaseRelocType.Reserved -> ()
            | BaseRelocType.ImageRelBasedAbsolute -> ()
            | _ -> failwith "Unknow relocation type"
        )

    member private this.RelocateHandler(handler: BinHandler, isa: ISA, baseAddress: UInt64) =
        // get ImageBase offset
        use peReader = new BinaryReader(new MemoryStream(handler.FileInfo.BinReader.Bytes))
        peReader.BaseStream.Seek(0x3CL, SeekOrigin.Begin) |> ignore
        let peOffset = peReader.ReadUInt32()
        let imageBaseOffset = peOffset + 0x34u
                
        // overwrite value in buffer
        use memStream = new MemoryStream(handler.FileInfo.BinReader.Bytes |> Array.copy)
        use binWriter = new BinaryWriter(memStream)
        binWriter.Seek(int32 imageBaseOffset, SeekOrigin.Begin) |> ignore
        binWriter.Write(uint32 baseAddress)
        binWriter.Close()        

        // re-create handler by invoking full init method via reflection if necessary
        let content = memStream.ToArray()
        match this.FileName with
        | Some filename ->
            let initMethod = handler.GetType().GetMethod("Init", BindingFlags.Static ||| BindingFlags.NonPublic)
            initMethod.Invoke(null, [|isa; ArchOperationMode.NoMode; true; baseAddress; content; filename|]) :?> BinHandler
        | None -> 
            BinHandler.Init(isa, ArchOperationMode.NoMode, false, baseAddress, content)                

    member this.GetFullName() =
        match this.FileName with
        | Some _ -> ()
        | None -> this.FileName <- Some <| UnknowLibrary.getName()
        this.FileName.Value

    member this.GetLibraryName() =
        Path.GetFileName(this.GetFullName())

    member this.IsLoaded() =
        _isLoaded

    member this.Load(proc: IProcessContainer) =
        // create handler
        let isa = proc.GetActiveMemoryRegion().Handler.ISA
        let mutable handler = 
            match this.FileName with
            | Some filename -> BinHandler.Init(isa, ArchOperationMode.NoMode, false, Addr.MinValue, filename)
            | None -> BinHandler.Init(isa, ArchOperationMode.NoMode, false, Addr.MinValue, content)

        // map the file
        match handler.FileInfo.FileFormat with
        | FileFormat.PEBinary ->
            Helpers.getPe(handler)
            |> Option.iter(fun pe ->
                let peHeader = pe.PEHeaders.PEHeader
                let nextLibFreeBaseAddress = 
                    proc.Memory.GetNextLibraryAllocationBase(peHeader.SizeOfImage, peHeader.ImageBase)
            
                let (mustRelocate, baseAddress) =
                    if nextLibFreeBaseAddress <> peHeader.ImageBase then (true, nextLibFreeBaseAddress)
                    else (false, peHeader.ImageBase)  
                            
                if mustRelocate then
                    // In case of relocation, recreate the handler with the correct address values                
                    handler <- this.RelocateHandler(handler, isa, baseAddress)                    

                // map library            
                Utility32.mapPeAtAddress(handler, proc.Memory, baseAddress)
                this.SetProperties(handler, baseAddress) 

                // must relocate the library if necessary
                if mustRelocate then
                    Helpers.getPe(handler)
                    |> Option.iter(fun pe -> this.ApplyRelocation(pe, proc, baseAddress))                    

                // add exported symbol to symbol list
                this.Exports |> Seq.iter(proc.SetSymbol)
            )
        | _ -> ()