namespace ES.Sojobo

open System
open B2R2.FrontEnd
open B2R2.BinIR.LowUIR

module LowUIREmulator =
    let rec emulateExpr(win32Process: Win32ProcessContainer) (expr: Expr) =
        ()

    and emulateStmt(win32Process: Win32ProcessContainer) (stmt: Stmt) =
        match stmt with
        | ISMark _ -> ()
        | IEMark _ -> ()
        //| LMark of Symbol
        | Put (destination, source) -> ()
        (*| Store of Endian * Expr * Expr
        | Jmp of Expr
        | CJmp of Expr * Expr * Expr
        | InterJmp of Expr * Expr
        | InterCJmp of Expr * Expr * Expr * Expr
        | SideEffect of SideEffect
        *)
        | _ -> failwith("Instruction not yet emulated: " + stmt.ToString())

    let emulateStmts(win32Process: Win32ProcessContainer) (stmts: Stmt array) =
        stmts 
        |> Array.iter(emulateStmt win32Process)

