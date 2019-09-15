namespace ES.Sojobo

open System
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open B2R2.BinFile.PE
open System.Collections.Generic

type NativeLibrary(content: Byte array) =
    let mutable _isLoaded = false

    member val Filename: String option = None with get, set
    member val Content = content with get
    member val EntryPoint = 0UL with get, set
    member val BaseAddress = 0UL with get, set
    member val Exports = new List<Symbol>() with get, set
    
    static member Create(content: Byte array) =
        new NativeLibrary(content)

    static member Create(filename: String) =
        let content = File.ReadAllBytes(filename)
        new NativeLibrary(content, Filename = Some filename)

    override this.ToString() =
        String.Format("{0}:0x{1}", defaultArg this.Filename "N/A", this.BaseAddress.ToString("X"))

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
                        LibraryName = (defaultArg this.Filename String.Empty) |> Path.GetFileName
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

    member internal this.GetLibraryName() =
        Path.GetFileName(this.Filename.Value)

    member this.IsLoaded() =
        _isLoaded

    member internal this.Load(proc: IProcessContainer) =
        // create handler
        let isa = ISA.OfString "x86"
        let mutable handler = 
            match this.Filename with
            | Some filename -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)
            | None -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, content)

        // map the file
        match handler.FileInfo.FileFormat with
        | FileFormat.PEBinary ->
            let pe = Helpers.getPe(handler)

            let (mustRelocate, baseAddress) =
                if proc.Memory.IsAddressMapped(pe.PEHeaders.PEHeader.ImageBase) 
                then (true, proc.Memory.GetFreeMemory(pe.PEHeaders.PEHeader.SizeOfImage, pe.PEHeaders.PEHeader.ImageBase))
                else (false, pe.PEHeaders.PEHeader.ImageBase)

            // map library            
            Utility.mapPeHeaderAtAddress(baseAddress, handler, proc.Memory)
            Utility.mapSectionsAtAddress(baseAddress, handler, proc.Memory)
            this.SetProperties(handler, baseAddress) 

            // must relocate the library if necessary
            if mustRelocate then                                
                this.ApplyRelocation(pe, proc, baseAddress)

            baseAddress                
        | _ ->
            0UL