namespace ES.Tengu

open System
open System.Collections.Generic
open System.IO
open ES.Sojobo
open ES.Sojobo.Model
open ES.Fslog
open B2R2

type ExecutedMemory = {
    Region: MemoryRegion
    StartAddress: UInt32
}

type internal Dumper(sandbox: ISandbox) =
    let _memoryRegions = new List<MemoryRegion>()
    let _unpackedRegion = new Dictionary<UInt64, ExecutedMemory>()

    let _logger =
        log "Dumper"
        |> info "MemoryDumped" "Dynamic code dumped to: {0}"
        |> build    

    let getRegions() =
        _unpackedRegion.Values
        |> Seq.toArray
    
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
        | Write(address, value) -> 
            proc.Memory.GetMemoryRegion(address)
            |> Option.iter(_memoryRegions.Add)
        | Allocate memRegion -> _memoryRegions.Add(memRegion)
        | Free _ -> ()

    member this.ConfigureLogger(logProvider: ILogProvider) =
        logProvider.AddLogSourceToLoggers(_logger)

    member this.SaveInformation() =
        getRegions()
        |> Array.iter(fun memRegion ->            
            let filename = String.Format("mem_dump_{0}.bin", memRegion.StartAddress.ToString("X"))
            let file = Path.Combine(Utility.getResultDir(sandbox.GetRunningProcess().Pid), filename)
            File.WriteAllBytes(file, memRegion.Region.Content)
            _logger?MemoryDumped(file)
        )