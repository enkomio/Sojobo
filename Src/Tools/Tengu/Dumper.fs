namespace ES.Tengu

open System
open System.Collections.Generic
open ES.Sojobo
open ES.Sojobo.Model
open B2R2

type ExecutedMemory = {
    Region: MemoryRegion
    StartAddress: UInt32
}

type internal Dumper() =
    let _memoryRegions = new List<MemoryRegion>()
    let _unpackedRegion = new Dictionary<UInt64, ExecutedMemory>()
    
    member this.Step(pc: UInt32) =
        _memoryRegions
        |> Seq.filter(fun memRegion -> _unpackedRegion.ContainsKey(memRegion.BaseAddress) |> not)
        |> Seq.tryFind(fun memRegion -> 
            pc >= uint32 memRegion.BaseAddress &&
            pc < uint32 memRegion.BaseAddress + uint32 memRegion.Content.Length
        )
        |> Option.iter(fun memRegion ->
            _unpackedRegion.[memRegion.BaseAddress] <- {Region = memRegion; StartAddress = pc}
        )

    member this.MemoryAccessedHandler(proc: IProcessContainer) (operation: MemoryAccessOperation) =
        match operation with
        | Read _ -> ()
        | Write(address, value) -> _memoryRegions.Add(proc.Memory.GetMemoryRegion(address))
        | Allocate memRegion -> _memoryRegions.Add(memRegion)
        | Free _ -> ()

    member this.GetRegions() =
        _unpackedRegion.Values
        |> Seq.toArray