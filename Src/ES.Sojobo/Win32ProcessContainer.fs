namespace ES.Sojobo

open System
open System.Collections.Generic
open System.IO
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR
open B2R2.BinFile
open B2R2.BinIR.LowUIR

type Win32ProcessContainer() =
    let _va = new List<MemoryRegion>()
    let mutable _handler: BinHandler option = None        

    let mapSections() =
        _handler.Value.FileInfo.GetSections()
        |> Seq.map(fun section -> {
            BaseAddress = int64 section.Address
            Protection = section.Kind
            Type = String.Empty
            Info = _handler.Value.FileInfo.FilePath
            Data = Array.zeroCreate<Byte>(int32 section.Size)
        })
        |> _va.AddRange

    member this.Initialize(buffer: Byte array) =
        let isa = ISA.OfString "x86"
        _handler <- Some <| BinHandler.Init(isa, ArchOperationMode.NoMode, FileFormat.RawBinary, Addr.MinValue, buffer)
        mapSections()

    member this.Initialize(filename: String) =  
        let isa = ISA.OfString "x86"
        _handler <- Some <| BinHandler.Init(isa, ArchOperationMode.NoMode, FileFormat.PEBinary, Addr.MinValue, filename)
        mapSections()

    member this.GetProcessHandler() =
        _handler.Value

        
