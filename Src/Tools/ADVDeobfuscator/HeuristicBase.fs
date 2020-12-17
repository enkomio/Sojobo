namespace ES.ADVDeobfuscator

open System
open Entities
open B2R2.FrontEnd.Intel

[<AbstractClass>]
type HeuristicBase() =
    abstract IgnorePrecondition: Boolean with get, set
    abstract IsSatisfied: unit -> Boolean
    abstract AnalyzeInstruction: Function * IntelInstruction -> unit
    abstract GetTrace: Function -> ObfuscationTrace
    abstract DeobfuscationFound: unit -> Boolean
    abstract HasFlag: DeobfuscationFlag -> Boolean
    abstract Reset: unit -> unit

