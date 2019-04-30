namespace ES.Sojobo

open System

type ISandbox =
    interface 
        abstract Create: String -> unit
        abstract Create: Byte array -> unit  
        abstract Run: unit -> unit
        abstract Stop: unit -> unit     
        abstract GetRunningProcess: unit -> IProcessContainer
    end

