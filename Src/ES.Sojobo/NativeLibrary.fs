namespace ES.Sojobo

open System
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open B2R2.BinFile.PE
open System.Collections.Generic
open ES.Sojobo.Model

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
        let pe = Helpers.getPe(handler)
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
        
    member private this.ApplyRelocation(pe: PE, proc: IProcessContainer, baseAddress: UInt64) =    
        let libraryAddress = baseAddress - pe.PEHeaders.PEHeader.ImageBase

        pe.RelocBlocks
        |> Seq.collect(fun block -> block.Entries |> Seq.map(fun entry -> (block, entry)))
        |> Seq.iter(fun (block, entry) ->
            let address = baseAddress + uint64 block.PageRVA + uint64 entry.Offset                
            let patch = (address + libraryAddress)
            
            match entry.Type with
            | BaseRelocType.ImageRelBasedHighlow ->                 
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint32 patch), false)
            | BaseRelocType.ImageRelBasedDir64 ->
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(patch), false)
            | BaseRelocType.ImageRelBasedHigh -> 
                let patch = (patch >>> 16) &&& 0xFFFFUL
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint16 patch), false)
            | BaseRelocType.ImageRelBasedLow -> 
                proc.Memory.WriteMemory(address, BitConverter.GetBytes(uint16 patch), false)
            
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

    member this.GetLibraryName() =
        match this.FileName with
        | Some _ -> ()
        | None -> this.FileName <- Some <| UnknowLibrary.getName()
        Path.GetFileName(this.FileName.Value)

    member this.IsLoaded() =
        _isLoaded

    member this.Load(proc: IProcessContainer) =
        // create handler
        let isa = 
            if proc.GetPointerSize() = 32 then ISA.OfString "x86"
            else ISA.OfString "x64"

        let mutable handler = 
            match this.FileName with
            | Some filename -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)
            | None -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, content)

        // map the file
        match handler.FileInfo.FileFormat with
        | FileFormat.PEBinary ->
            let pe = Helpers.getPe(handler)
            let peHeader = pe.PEHeaders.PEHeader
            let nextLibFreeBaseAddress = proc.Memory.GetNextLibraryAllocationBase(peHeader.SizeOfImage, peHeader.ImageBase)

            let (mustRelocate, baseAddress) =
                if proc.Memory.IsAddressMapped(peHeader.ImageBase) || peHeader.ImageBase < nextLibFreeBaseAddress
                then (true, nextLibFreeBaseAddress)
                else (false, peHeader.ImageBase)            

            // map library            
            Utility32.mapPeHeaderAtAddress(baseAddress, handler, proc.Memory)
            Utility32.mapSectionsAtAddress(baseAddress, handler, proc.Memory)
            this.SetProperties(handler, baseAddress) 

            // add exported symbol to symbol list
            this.Exports |> Seq.iter(proc.SetSymbol)           

            // must relocate the library if necessary
            if mustRelocate then                                
                this.ApplyRelocation(pe, proc, baseAddress)
        | _ -> ()