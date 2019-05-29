namespace ES.Sojobo

open System
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR.LowUIR
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2.BinIR

type LowUIREmulator(sandbox: BaseSandbox) =
    let rec emulateExpr(baseProcess: BaseProcessContainer) (expr: Expr) =
        match expr with
        | TempVar (regType, index) ->
            baseProcess.GetOrCreateTemporaryVariable(string index, Utility.getType(regType))

        | Num number ->
            let size = Utility.getType(BitVector.getType number)
            createVariableWithValue(String.Empty, size, number)

        | Var (regType, registerId, _, _) ->
            let register = Register.ofRegID registerId            
            baseProcess.GetVariable(string register, Utility.getType(regType))

        | BinOp (binOpType, regType, firstOp, secondOp, _, _) ->
            let firstValue = emulateExpr baseProcess firstOp
            let secondValue = emulateExpr baseProcess secondOp           

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
                | _ -> failwith("Wrong or unsupported operation")
            
            let resultValue = operation firstValue.Value secondValue.Value
            createVariableWithValue(String.Empty, Utility.getType(regType), resultValue)

        | Load (_, regType, expr, _, _) ->             
            let memAddressValue = (emulateExpr baseProcess expr).Value
            let memAddress = BitVector.toUInt64 memAddressValue
            let emuType = Utility.getType(regType)
            let numBytes = Utility.getSize(emuType) / 8
            let bytes = baseProcess.Memory.ReadMemory(memAddress, numBytes)
                        
            // convert the readed bytes to emulated value
            match emuType with
            | Byte -> uint32 bytes.[0] |> bigint
            | Word -> uint32(BitConverter.ToUInt16(bytes, 0)) |> bigint
            | DoubleWord -> uint32(BitConverter.ToUInt32(bytes, 0)) |> bigint
            | QuadWord -> uint64(BitConverter.ToUInt64(bytes, 0)) |> bigint
            | _ -> failwith("Unexpected emu type")
            |> fun bi -> createVariableWithValue(String.Empty,  Utility.getType(regType), BitVector.ofUBInt bi regType)

        | PCVar (regType, regName) ->
            baseProcess.GetVariable(regName, Utility.getType(regType))

        | RelOp (relOpType, firstExpr, secondExpr, exprInfo, consInfo) ->
            let firstValue = emulateExpr baseProcess firstExpr
            let secondValue = emulateExpr baseProcess secondExpr 
            
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
                | _ -> failwith("Wrong or unsupported operation")

            let resultValue = operation firstValue.Value secondValue.Value
            createVariableWithValue(String.Empty, firstValue.Type, resultValue)

        | Extract(targetExpr, regType, startPos, _, _) ->
            let targetValue = emulateExpr baseProcess targetExpr
            let extractionResult = BitVector.extract targetValue.Value regType startPos
            createVariableWithValue(String.Empty, Utility.getType(regType), extractionResult)

        | UnOp (unOpType, targetExpr, _, _) ->
            let operation =
                match unOpType with
                | UnOpType.NEG -> BitVector.neg
                | UnOpType.NOT -> BitVector.bnot
                | _ -> failwith("Wrong or unsupported operation")

            let value = emulateExpr baseProcess targetExpr
            let resultValue = operation value.Value
            createVariableWithValue(String.Empty, value.Type, resultValue)

        | Undefined (regType, txt) ->
            createVariable(String.Empty, Utility.getType(regType))
                          
        | Cast (castKind, regType, expr, exprInfo, consInfo)->
            let valueToCast = emulateExpr baseProcess expr
            let castedValue =
                match castKind with
                | CastKind.SignExt -> BitVector.sext valueToCast.Value regType
                | CastKind.ZeroExt -> BitVector.zext valueToCast.Value regType
                | _ -> raise IllegalASTTypeException
            
            {valueToCast with 
                Type = Utility.getType(regType)
                Value = castedValue
            }

        | Ite (conditionExpr, trueExpression, falseExpression, _, _) ->
            let conditionValue = emulateExpr baseProcess conditionExpr
            if BitVector.isTrue conditionValue.Value 
            then emulateExpr baseProcess trueExpression
            else emulateExpr baseProcess falseExpression

        | Name(name, n) ->
            createVariable(name, EmulatedType.DoubleWord)

        // | FuncName of string  
        | _ -> failwith("Expression not yet emulated")

    and emulateStmt(state: EmulatorExecutionState, stmt: Stmt) =
        match stmt with
        | ISMark _ -> ()
        | IEMark _ ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            baseProcess.ClearTemporaryVariables()

        | Put (destination, source) -> 
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let sourceValue = emulateExpr baseProcess source
            let destinationValue = 
                {emulateExpr baseProcess destination with
                    Value = sourceValue.Value
                }
            baseProcess.SetRegister(destinationValue)

        | Store (_, destination, source) ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let sourceValue = emulateExpr baseProcess source
            let destinationValue = emulateExpr baseProcess destination

            // extract info
            let memAddress = BitVector.toUInt64 destinationValue.Value          
            let bytes = Utility.toArray(sourceValue.Value)
            
            // write value
            baseProcess.Memory.WriteMemory(memAddress, bytes)
            
        | InterJmp (programCounterExpr, destAddrExpr, interJumpInfo) ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let destAddr = emulateExpr baseProcess destAddrExpr
            let programCounter = 
                {emulateExpr baseProcess programCounterExpr with
                    Value = destAddr.Value
                }
            baseProcess.SetRegister(programCounter)
            
            // update the active memory region
            let destMemRegion = baseProcess.Memory.GetMemoryRegion(programCounter.Value |> BitVector.toUInt64)
            baseProcess.UpdateActiveMemoryRegion(destMemRegion)

            // stop execution, see: https://github.com/B2R2-org/B2R2/issues/15#issuecomment-496872936
            state.Stop()

        | InterCJmp (conditionExpr, currentProgramCounter, trueDestAddrExpr, falseDesAddrExpr) ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let conditionValue = emulateExpr baseProcess conditionExpr
            {baseProcess.ProgramCounter with
                Value =
                    if BitVector.isTrue conditionValue.Value
                    then (emulateExpr baseProcess trueDestAddrExpr).Value
                    else (emulateExpr baseProcess falseDesAddrExpr).Value
            }
            |> baseProcess.SetRegister

            // update the active memory region
            let destMemRegion = baseProcess.Memory.GetMemoryRegion(baseProcess.ProgramCounter.Value |> BitVector.toUInt64)
            baseProcess.UpdateActiveMemoryRegion(destMemRegion)

            // stop execution, see: https://github.com/B2R2-org/B2R2/issues/15#issuecomment-496872936
            state.Stop()
            
        | SideEffect sideEffect ->
            sandbox.TriggerSideEffect(sideEffect)
        
        | CJmp(conditionExpr, trueDestAddrExpr, falseDesAddrExpr) ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let conditionValue = emulateExpr baseProcess conditionExpr
            let label =
                if BitVector.isTrue conditionValue.Value 
                then (emulateExpr baseProcess trueDestAddrExpr).Name
                else (emulateExpr baseProcess falseDesAddrExpr).Name
                
            // jump to given statement
            state.JumpTo(label)

        | LMark(_) ->
            // this statement was already considered
            ()
             
        | Jmp(labelExpr) ->
            let baseProcess = sandbox.GetRunningProcess() :?> BaseProcessContainer
            let label = (emulateExpr baseProcess labelExpr).Name

            // jump to given statement
            state.JumpTo(label)
        
    member this.Emulate(stmts: Stmt array) =
        let state = new EmulatorExecutionState(stmts)
        while state.HasMoreStatement() do
            let stmt = state.GetStatement()
            emulateStmt(state, stmt)

    member this.EmulateInstruction(handler: BinHandler, instruction: Instruction) =
        BinHandler.LiftInstr handler instruction
        |> BinHandler.Optimize
        |> this.Emulate

    interface IEmulator with
        member this.Emulate(stmts: Stmt array) =
            this.Emulate(stmts)

        member this.EmulateInstruction(handler: BinHandler, instruction: Instruction) =
            this.EmulateInstruction(handler, instruction)