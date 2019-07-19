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

    member private this.SetProperties(handler: BinHandler) =
        let pe = Helpers.getPe(handler)
        this.SetProperties(
            uint64 pe.PEHeaders.PEHeader.AddressOfEntryPoint, 
            uint64 pe.PEHeaders.PEHeader.ImageBase,
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
        
    member private this.Relocate(pe: PE, handler: BinHandler) =
        // TODO: to be implemented
        //let relocSymbol = handler.FileInfo.GetRelocationSymbols()
        handler

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
            if proc.Memory.IsAddressMapped(pe.PEHeaders.PEHeader.ImageBase) then
                // must relocate the library
                handler <- this.Relocate(pe, handler)
            else
                Utility.mapPeHeader(handler, proc.Memory)
                Utility.mapSections(handler, proc.Memory)
                this.SetProperties(handler) 
        | _ -> ()