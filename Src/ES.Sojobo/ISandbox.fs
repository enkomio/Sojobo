namespace ES.Sojobo

open System

type ISandbox =
    interface 
        abstract Run: String -> unit
        abstract Run: Byte array -> unit
        abstract AddCallback: functionName: String * moduleName: String * callback: Action<IProcessContainer> -> unit
    end

