namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2.BinIR.LowUIR
open B2R2.FrontEnd

type InstructionCache() =
    let _mostAccessed = new Dictionary<UInt64, Int32>()
    let _cache = new Dictionary<UInt64, (Instruction * Stmt array)>()

    let removeLast() =
        if _cache.Count > 0 then
            let lastKv = _cache |> Seq.last
            _cache.Remove(lastKv.Key) |> ignore

    let purgeCache() =
        let averageAccess = 
            _mostAccessed.Values 
            |> Seq.cast<float>
            |> Seq.average
            |> int32

        let toRemove =
            _mostAccessed
            |> Seq.filter(fun kv -> kv.Value <= averageAccess)
            |> Seq.toArray

        // remove elements
        if toRemove.Length > 0 then
            toRemove |> Array.iter(fun addr -> _mostAccessed.Remove(addr.Key) |> ignore)
            toRemove |> Array.iter(fun addr -> _cache.Remove(addr.Key) |> ignore)
        else
            removeLast()

    member this.IsCached(pc: UInt64) =
        _cache.ContainsKey(pc)

    member this.GetCachedInstruction(pc: UInt64) =
        _mostAccessed.[pc] <- _mostAccessed.[pc] + 1
        _cache.[pc]

    member this.CacheInstruction(pc: UInt64, instruction: Instruction, stmts: Stmt array) =
        if _cache.Count > 50000 then purgeCache()            
        _cache.[pc] <- (instruction, stmts)
        _mostAccessed.[pc] <- 1