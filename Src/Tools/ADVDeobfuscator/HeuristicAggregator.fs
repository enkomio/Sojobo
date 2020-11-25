namespace ES.ADVDeobfuscator

open System
open System.Reflection
open System.Collections.Generic
open B2R2.FrontEnd.Intel
open ES.ADVDeobfuscator.Entities
open B2R2
open ES.Fslog

type HeuristicAggregator(functionAddresses: UInt64 array, logProvider: ILogProvider) =
    inherit HeuristicBase()

    let _heuristics: HeuristicBase list = [
        new ArithmeticWithStackHeuristic(functionAddresses, logProvider)
    ]        

    new (logProvider: ILogProvider) = new HeuristicAggregator(Array.empty, logProvider)

    default this.IsSatisfied() =
        _heuristics
        |> List.exists(fun heuristic -> heuristic.IsSatisfied())

    default this.GetTrace(func: Function) =
        _heuristics
        |> List.find(fun heuristic -> heuristic.IsSatisfied())
        |> fun heuristic -> heuristic.GetTrace(func)

    default this.DeobfuscationFound() =
        _heuristics
        |> List.exists(fun heuristic -> heuristic.DeobfuscationFound())

    default this.HasFlag(flag: DeobfuscationFlag) =
        _heuristics
        |> List.exists(fun heuristic -> heuristic.HasFlag(flag))

    default this.Reset() =
        _heuristics
        |> List.iter(fun heuristic -> heuristic.Reset())

    default this.AnalyzeInstruction(func: Function, instruction: IntelInstruction) =    
        _heuristics
        |> List.iter(fun heuristic -> heuristic.AnalyzeInstruction(func, instruction))