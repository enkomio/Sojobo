namespace ES.Sojobo

open System
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile.PE

type NativeLibrary(content: Byte array) =
    member val Filename: String option = None with get, set
    member val Content = content with get
    member val EntryPoint = 0UL with get, set
    member val BaseAddress = 0UL with get, set
    member val Exports = Map.empty<UInt64, String> with get, set
    
    static member Create(content: Byte array) =
        new NativeLibrary(content)

    static member Create(filename: String) =
        let content = File.ReadAllBytes(filename)
        new NativeLibrary(content, Filename = Some filename)

    member private this.SetProperties(handler: BinHandler) =
        let pe = Helpers.getPe(handler)
        this.EntryPoint <- uint64 pe.PEHeaders.PEHeader.AddressOfEntryPoint
        this.BaseAddress <- uint64 pe.PEHeaders.PEHeader.ImageBase
        this.Exports <- pe.ExportMap     
        
    member private this.Relocate(pe: PE, handler: BinHandler) =
        // TODO: to be implemented
        handler

    member internal this.GetLibraryName() =
        Path.GetFileName(this.Filename.Value)

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
            
            Utility.mapPeHeader(handler, proc.Memory)
            Utility.mapSections(handler, proc.Memory)
            this.SetProperties(handler)            
        | _ -> ()