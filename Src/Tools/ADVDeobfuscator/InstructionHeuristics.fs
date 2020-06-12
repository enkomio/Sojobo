namespace ES.ADVDeobfuscator

open System
open System.Collections.Generic
open B2R2.FrontEnd.Intel
open ES.ADVDeobfuscator.Entities
open B2R2

type InstructionHeuristics(functionAddreses: UInt64 array) =
    let getMovdqaToStackRdataAddress(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.MOVDQA ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprReg opReg, OprMem (Some reg, scale, Some disposition, opSize)) when opReg = Register.XMM0 -> 
                        let ripAddress = fst(instruction.GetNextInstrAddrs() |> Seq.head)
                        let address = ripAddress + uint64 disposition
                        Some address
                | _ -> None
            | _ -> None
        | _ -> None      

    let registers8bit = 
        let tmp = new List<Register>()
        for regValue in Enum.GetValues(typeof<Register>) do 
            try
                let register = regValue :?> Register
                if Register.toRegType(register) = 8<rt> then            
                    tmp.Add(register)
            with _ -> ()
        tmp

    let checkXorWith8bitRegisterHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.XOR ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprReg opReg1, OprReg opReg2) when opReg1 = opReg2 -> List.empty
                | (_, OprReg opReg) 
                | (OprReg opReg, _) when (registers8bit.Contains(opReg)) ->  [InstructionFlags.XorWith8BitRegister]
                | _ -> List.empty
            | _ -> List.empty
        | _ -> List.empty

    let checkXorWithStackHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.XOR ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, scale, Some disposition, opSize), OprReg opReg) 
                    when (regValue = Register.RBP || regValue = Register.RSP) && (registers8bit.Contains(opReg)) -> [InstructionFlags.XorWithStack]
                | _ -> List.empty
            | _ -> List.empty
        | _ -> List.empty

    let checkAddWithImmediateHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with      
        | Opcode.ADD ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, scale, Some disposition, opSize), OprImm opImm) 
                    when (regValue = Register.RBP || regValue = Register.RSP || opSize = 8<rt>) && opImm <= 255L  -> [InstructionFlags.AddStackWithImmediate]
                | _ -> List.empty
            | _ -> List.empty        
        | _ -> List.empty

    let checkIsJumpToLoopHeuristic(instruction: IntelInstruction) =
        if instruction.Info.Opcode = Opcode.JB then
            [InstructionFlags.JumpBelow; InstructionFlags.JumpToLoopEdge]
        elif instruction.Info.Opcode = Opcode.JMPNear then
             match instruction.Info.Operands with
             | OneOperand(OprDirAddr(Relative offset)) when offset < 0L -> [InstructionFlags.JumpToPreviousAddress; InstructionFlags.JumpToLoopEdge]                
             | _ -> List.empty
        else
            List.empty

    let checkSetByteRegisterInStackHeuristic(instruction: IntelInstruction) =  
        match instruction.Info.Opcode with
        | Opcode.MOV ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, scale, Some disposition, opSize), OprReg opReg) 
                    when (regValue = Register.RBP || regValue = Register.RSP) && (registers8bit.Contains(opReg)) -> [InstructionFlags.SetByteRegisterInStack]
                | _ -> List.empty
            | _ -> List.empty
        | _ -> List.empty

    let checkMovToStackHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.MOV ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, scale, Some disposition, opSize), OprImm opImm) 
                    when regValue = Register.RBP || regValue = Register.RSP -> [InstructionFlags.MovToStack]
                | _ -> List.empty
            | _ -> List.empty
        | _ -> List.empty

    let checkMovdqaToStackHeuristic(instruction: IntelInstruction) =
        match getMovdqaToStackRdataAddress(instruction) with
        | Some _ -> [InstructionFlags.MovdqaToStack]
        | _ -> List.empty

    let checkCallToFunctionWithDecryptionHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.CALLNear ->
            match instruction.Info.Operands with
            | OneOperand(OprDirAddr(Relative offset)) ->
                let address = uint64(int64 instruction.Address + offset)
                if functionAddreses |> Array.contains address then [InstructionFlags.CallToFunctionDecrypt]
                else List.empty
            | _ -> List.empty
        | _ -> List.empty

    let checkCallToExtraneousFunctionHeuristic(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.CALLNear ->
            match instruction.Info.Operands with
            | OneOperand(OprDirAddr(Relative offset)) ->
                let address = uint64(int64 instruction.Address + offset)
                if functionAddreses |> Array.contains address |> not then [InstructionFlags.CallToExtraneousFunction]
                else List.empty
            | _ -> List.empty
        | _ -> List.empty

    new () = new InstructionHeuristics(Array.empty)

    member this.AnalyzeInstruction(func: Function, instruction: IntelInstruction) =
        List.concat[
            checkMovdqaToStackHeuristic(instruction)
            checkMovToStackHeuristic(instruction)
            checkSetByteRegisterInStackHeuristic(instruction)
            checkIsJumpToLoopHeuristic(instruction)
            checkAddWithImmediateHeuristic(instruction)
            checkXorWithStackHeuristic(instruction)
            checkXorWith8bitRegisterHeuristic(instruction)
            checkCallToFunctionWithDecryptionHeuristic(instruction)   
            checkCallToExtraneousFunctionHeuristic(instruction)
        ]
        |> List.fold(fun state f -> state ||| f) InstructionFlags.Empty