namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2.BinIR.LowUIR

type internal EmulatorExecutionState(stmts: Stmt array) =
    let mutable _index = 0
    let _labels = new Dictionary<String, Int32>()
    let _stmts = new Dictionary<Int32, Stmt>()

    let initialize() =
        stmts
        |> Array.iteri(fun index stmt ->            
            _stmts.[index] <- stmt

            // verify if a label is defined
            match stmt with 
            | LMark(name, n) -> _labels.[name] <- index
            | _ -> ()
        )

    do initialize()

    member this.GetStatement() =
        let stmt = _stmts.[_index]
        _index <- _index + 1
        stmt

    member this.HasMoreStatement() =
        _index < _stmts.Count

    member this.Stop() =
        _index <- _stmts.Count

    member this.JumpTo(label: String) =
        _index <- _labels.[label]