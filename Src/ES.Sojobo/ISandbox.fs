namespace ES.Sojobo

open System
open B2R2.BinIR

exception UnhandledFunction of string

type ISandbox =
    interface 
        abstract Load: String -> unit
        abstract Load: Byte array -> unit  
        abstract Run: unit -> unit
        abstract Stop: unit -> unit     
        abstract GetRunningProcess: unit -> IProcessContainer
        abstract SideEffect: IEvent<ISandbox * SideEffect> with get
    end