namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2.FrontEnd

type MemoryManager() =
    let _va = new Dictionary<UInt64, MemoryRegion>() 
    let _memoryAccessedEvent = new Event<MemoryAccessOperation>()

    member this.MemoryAccessed = _memoryAccessedEvent.Publish   
    
    member this.ReadMemory(address: UInt64, size: Int32) =
        // TODO: add check on memory protection
        _memoryAccessedEvent.Trigger(MemoryAccessOperation.Read address)
        let memRegion = this.GetMemoryRegion(address)
        BinHandler.ReadBytes(memRegion.Handler, address, size)

    member internal this.UnsafeWriteMemory(address: UInt64, value: Byte array, verifyProtection: Boolean) =        
        let region = this.GetMemoryRegion(address)
        if verifyProtection then    
            // TODO: add check on memory protection
            ()

        let offset = region.Handler.FileInfo.TranslateAddress address
        Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)

    member this.WriteMemory(address: UInt64, value: Byte array) =
        _memoryAccessedEvent.Trigger(MemoryAccessOperation.Write address)
        this.UnsafeWriteMemory(address, value, true)

    member this.UpdateMemoryRegion(baseAddress: UInt64, memoryRegion: MemoryRegion) =
        _va.[baseAddress] <- memoryRegion

    member this.GetMemoryRegion(address: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address < endAddr
        )

    member this.AddMemoryRegion(memRegion: MemoryRegion) =
        _va.[memRegion.BaseAddress] <- memRegion

    member this.GetMemoryMap() =
        _va.Values 
        |> Seq.sortBy(fun m -> m.BaseAddress)
        |> Seq.readonly 
        |> Seq.toArray

    member this.FreeMemoryRegion(address: UInt64) =        
        let region = this.GetMemoryRegion(address)
        _memoryAccessedEvent.Trigger(MemoryAccessOperation.Free region)
        _va.Remove(region.BaseAddress)        

    member this.AllocateMemory(size: Int32, protection: MemoryProtection) =
        let baseAddress =
            this.GetMemoryMap()
            |> Seq.pairwise
            |> Seq.tryFind(fun (m1, m2) ->
                let availableSize = m2.BaseAddress - (m1.BaseAddress + uint64 m1.Content.Length)
                availableSize > uint64 size
            )
            |> function
                | Some (m1, _) -> 
                    m1.BaseAddress + uint64 m1.Content.Length
                | None -> 
                    let lastRegion = this.GetMemoryMap() |> Array.last
                    lastRegion.BaseAddress + uint64 lastRegion.Content.Length

        // create the memory region
        let region = createMemoryRegion(baseAddress, size, protection)
        _memoryAccessedEvent.Trigger(MemoryAccessOperation.Allocate region)
        this.AddMemoryRegion(region)

        baseAddress