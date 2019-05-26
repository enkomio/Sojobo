namespace ES.Sojobo

open System
open System.Reflection
open System.IO
open B2R2
open B2R2.FrontEnd
open ES.Sojobo.Model

type NativeLibrary(content: Byte array) =
    member val Filename: String option = None with get, set
    member val Content = content with get
    member val EntryPoint = 0UL with get, set
    member val BaseAddress = 0UL with get, set
    
    static member Create(content: Byte array) =
        new NativeLibrary(content)

    static member Create(filename: String) =
        let content = File.ReadAllBytes(filename)
        new NativeLibrary(content, Filename = Some filename)

    member internal this.Load(proc: IProcessContainer) =
        // create handler
        let isa = ISA.OfString "x86"
        let handler = 
            match this.Filename with
            | Some filename -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)
            | None -> BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, content)

        // map the file
        match handler.FileInfo.FileFormat with
        | FileFormat.PEBinary ->
            Utility.mapPeHeader(handler, proc.Memory)
            Utility.mapSections(handler, proc.Memory)

            let pe = Utility.getPe(handler)
            this.EntryPoint <- uint64 pe.PEHeaders.PEHeader.AddressOfEntryPoint
            this.BaseAddress <- uint64 pe.PEHeaders.PEHeader.ImageBase

        | _ ->
            // just map the file in memory
            proc.Memory.AllocateMemory(content, MemoryProtection.Read)
            |> ignore