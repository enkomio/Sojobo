namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2.FrontEnd

type MemoryManager() =
    let _va = new Dictionary<UInt64, MemoryRegion>() 
    
    member this.ReadMemory(address: UInt64, size: Int32) =
        // TODO: add check on memory protection
        let memRegion = this.GetMemoryRegion(address)
        BinHandler.ReadBytes(memRegion.Handler, address, size)

    member this.WriteMemory(address: UInt64, value: Byte array) =
        // copy the memory
        let region = this.GetMemoryRegion(address)
        let offset = region.Handler.FileInfo.TranslateAddress address
        Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)

    member this.GetMemoryRegion(address: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address <= endAddr
        )

    member this.AddMemoryRegion(memRegion: MemoryRegion) =
        _va.[memRegion.BaseAddress] <- memRegion

    member this.GetMemoryMap() =
        _va.Values 
        |> Seq.readonly 
        |> Seq.toArray