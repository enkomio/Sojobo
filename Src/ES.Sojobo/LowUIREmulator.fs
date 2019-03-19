namespace ES.Sojobo

open System
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.LowUIR
open ES.Sojobo.Model
open B2R2.BinIR.LowUIR.AST
open B2R2.FrontEnd.Intel

module LowUIREmulator =
    let rec emulateExpr(win32Process: Win32ProcessContainer) (expr: Expr) =
        match expr with
        | TempVar (regType, index) ->
            win32Process.GetOrCreateTemporaryVariable(string index, Utility.getType(regType))

        | Num number ->
            let size = Utility.getType(BitVector.getType number)
            createVariableWithValue(String.Empty, size, number)

        | Var (regType, registerId, _, _) ->
            let register = Register.ofRegID registerId            
            win32Process.GetVariable(string register, Utility.getType(regType))

        | BinOp (binOpType, regType, firstOp, secondOp, _, _) ->
            let firstValue = emulateExpr win32Process firstOp
            let secondValue = emulateExpr win32Process secondOp           

            let operation =
                match binOpType with
                | BinOpType.ADD -> BitVector.add
                | BinOpType.SUB -> BitVector.sub
                | BinOpType.MUL -> BitVector.mul
                | BinOpType.DIV -> BitVector.div
                | BinOpType.SDIV -> BitVector.sdiv
                | BinOpType.MOD -> BitVector.modulo
                | BinOpType.SMOD -> BitVector.smodulo
                | BinOpType.SHL -> BitVector.shl
                | BinOpType.SHR -> BitVector.shr
                | BinOpType.SAR -> BitVector.sar
                | BinOpType.AND -> BitVector.band
                | BinOpType.OR -> BitVector.bor
                | BinOpType.XOR -> BitVector.bxor
                | BinOpType.CONCAT -> BitVector.concat
                | _ -> failwith("Wrong or unsupported operation: " + binOpType.ToString())
            
            let resultValue = operation firstValue.Value secondValue.Value
            createVariableWithValue(String.Empty, Utility.getType(regType), resultValue)

        | Load (_, regType, expr, _, _) -> 
            let memAddressValue = (emulateExpr win32Process expr).Value
            let memAddress = BitVector.toUInt64 memAddressValue
            let emuType = Utility.getType(regType)
            let size = Utility.getSize(emuType)

            let memRegion = win32Process.GetMemoryRegion(memAddress)
            let handler = memRegion.Handler
            let bytes = BinHandler.ReadBytes(handler, memAddress, size)
                        
            // convert the readed bytes to emulated value
            match emuType with
            | Byte -> uint32 bytes.[0] |> bigint
            | Word -> uint32(BitConverter.ToUInt16(bytes, 0)) |> bigint
            | DoubleWord -> uint32(BitConverter.ToUInt32(bytes, 0)) |> bigint
            | QuadWord -> uint64(BitConverter.ToUInt64(bytes, 0)) |> bigint
            |> fun bi -> createVariableWithValue(String.Empty,  Utility.getType(regType), BitVector.ofUBInt bi regType)

        | _ -> failwith("Expression not yet emulated: " + expr.ToString())

    and emulateStmt(win32Process: Win32ProcessContainer) (stmt: Stmt) =
        match stmt with
        | ISMark _ -> ()
        | IEMark _ ->
            win32Process.ClearTemporaryVariables()

        | Put (destination, source) -> 
            let sourceValue = emulateExpr win32Process source
            let destinationValue = 
                {emulateExpr win32Process destination with
                    Value = sourceValue.Value
                }
            win32Process.SetVariable(destinationValue)

        | Store (_, destination, source) ->
            let sourceValue = emulateExpr win32Process source
            let destinationValue = emulateExpr win32Process destination

            // extract info
            let memAddress = BitVector.toUInt64 destinationValue.Value
            let memRegion = win32Process.GetMemoryRegion(memAddress)
            let handler = memRegion.Handler
            let bytes = Utility.toArray(sourceValue.Value)
            
            // write value
            let newHandler = BinHandler.UpdateCode handler memAddress bytes
            let newRegion = {memRegion with Handler = newHandler}
            win32Process.UpdateMemoryRegion(memRegion, newRegion)
            
        (*
        | LMark of Symbol
        | Jmp of Expr
        | CJmp of Expr * Expr * Expr
        | InterJmp of Expr * Expr
        | InterCJmp of Expr * Expr * Expr * Expr
        | SideEffect of SideEffect
        *)
        | _ -> failwith("Statement not yet emulated: " + stmt.ToString())