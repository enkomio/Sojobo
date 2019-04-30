namespace ES.Sojobo

open System
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.LowUIR
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2.BinIR

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
            let numBytes = Utility.getSize(emuType) / 8

            let memRegion = win32Process.GetMemoryRegion(memAddress)            
            let bytes = BinHandler.ReadBytes(memRegion.Handler, memAddress, numBytes)
                        
            // convert the readed bytes to emulated value
            match emuType with
            | Byte -> uint32 bytes.[0] |> bigint
            | Word -> uint32(BitConverter.ToUInt16(bytes, 0)) |> bigint
            | DoubleWord -> uint32(BitConverter.ToUInt32(bytes, 0)) |> bigint
            | QuadWord -> uint64(BitConverter.ToUInt64(bytes, 0)) |> bigint
            | _ -> failwith("Unexpected emu type: " + emuType.ToString())
            |> fun bi -> createVariableWithValue(String.Empty,  Utility.getType(regType), BitVector.ofUBInt bi regType)

        | PCVar (regType, regName) ->
            win32Process.GetVariable(regName, Utility.getType(regType))

        | RelOp (relOpType, firstExpr, secondExpr, exprInfo, consInfo) ->
            let firstValue = emulateExpr win32Process firstExpr
            let secondValue = emulateExpr win32Process secondExpr 
            
            let operation =
                match relOpType with
                | RelOpType.EQ -> BitVector.eq
                | RelOpType.NEQ -> BitVector.neq
                | RelOpType.GT -> BitVector.gt
                | RelOpType.GE -> BitVector.ge
                | RelOpType.SGT -> BitVector.sgt
                | RelOpType.SGE -> BitVector.sge
                | RelOpType.LT -> BitVector.lt
                | RelOpType.LE -> BitVector.le
                | RelOpType.SLT -> BitVector.slt
                | RelOpType.SLE -> BitVector.sle
                | _ -> failwith("Wrong or unsupported operation: " + relOpType.ToString())

            let resultValue = operation firstValue.Value secondValue.Value
            createVariableWithValue(String.Empty, firstValue.Type, resultValue)

        | Extract(targetExpr, regType, startPos, _, _) ->
            let targetValue = emulateExpr win32Process targetExpr
            let extractionResult = BitVector.extract targetValue.Value regType startPos
            createVariableWithValue(String.Empty, Utility.getType(regType), extractionResult)

        | UnOp (unOpType, targetExpr, _, _) ->
            let operation =
                match unOpType with
                | UnOpType.NEG -> BitVector.neg
                | UnOpType.NOT -> BitVector.bnot
                | _ -> failwith("Wrong or unsupported operation: " + unOpType.ToString())

            let value = emulateExpr win32Process targetExpr
            let resultValue = operation value.Value
            createVariableWithValue(String.Empty, value.Type, resultValue)

        | Undefined (regType, txt) ->
            createVariable(String.Empty, Utility.getType(regType))

        // | Name of Symbol
        // | FuncName of string
        // | Ite of Expr * Expr * Expr * ExprInfo * ConsInfo option
        // | Cast of CastKind * RegType * Expr * ExprInfo * ConsInfo option

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
            let bytes = Utility.toArray(sourceValue.Value)
            
            // write value
            win32Process.WriteMemory(memAddress, bytes)
            
        | InterJmp (programCounterExpr, destAddrExpr, interJumpInfo) ->
            let destAddr = emulateExpr win32Process destAddrExpr
            let programCounter = 
                {emulateExpr win32Process programCounterExpr with
                    Value = destAddr.Value
                }
            win32Process.SetVariable(programCounter)

        | InterCJmp (conditionExpr, currentProgramCounter, trueDestAddrExpr, falseDesAddrExpr) ->
            let conditionValue = emulateExpr win32Process conditionExpr
            {win32Process.GetProgramCounter() with
                Value =
                    if BitVector.isPositive conditionValue.Value
                    then (emulateExpr win32Process trueDestAddrExpr).Value
                    else (emulateExpr win32Process falseDesAddrExpr).Value
            }
            |> win32Process.SetVariable
        (*
        | LMark of Symbol
        | Jmp of Expr
        | CJmp of Expr * Expr * Expr                
        | SideEffect of SideEffect
        *)
        | _ -> failwith("Statement not yet emulated: " + stmt.ToString())