namespace ES.ADVDeobfuscator

open System
open System.Collections.Generic
open B2R2.FrontEnd.Intel
open B2R2

[<RequireQualifiedAccess>]
module HeuristicHelper =
    let private _registerGroups =
        let tmp = new Dictionary<Register, List<Register>>()
        Enum.GetValues(typeof<Register>)
        |> Seq.cast<Register>
        |> Seq.iter(fun register ->
            try
                let extendedRegister = B2R2.FrontEnd.Intel.Register.extendRegister64 register
                if tmp.ContainsKey(extendedRegister) |> not then
                    tmp.[extendedRegister] <- new List<Register>()
                tmp.[extendedRegister].Add(register)
            with _ -> ()
        )
        tmp

    let private _registers8bit = 
        let tmp = new List<Register>()
        Enum.GetValues(typeof<Register>)
        |> Seq.cast<Register>
        |> Seq.iter(fun register ->
            try
               if Register.toRegType(register) = 8<rt> then            
                   tmp.Add(register)
            with _ -> ()
        )
        tmp

    let private _avxRegisters =
        let tmp = new List<Register>()
        for regValue in Enum.GetValues(typeof<Register>) do 
            try
                let register = regValue :?> Register
                if Register.toRegType(register) >= 128<rt> then            
                    tmp.Add(register)
            with _ -> ()
        tmp

    let private is8BitRegister(opReg: Register) =
        _registers8bit.Contains(opReg)

    let private isAVXRegister(opReg: Register) =
        _avxRegisters.Contains(opReg)

    let private isStackRegister(reg: Register) =
        [Register.EBP; Register.ESP; Register.RSP; Register.RBP]
        |> List.contains reg

    let isArithmeticStackMemoryWithImmediate(instruction: IntelInstruction, opCode: Opcode) =
        if instruction.Info.Opcode = opCode then
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, _, _, opSize), OprImm opImm) 
                    when (isStackRegister(regValue) || opSize = 8<rt>) && opImm <= 255L  -> true
                | _ -> false
            | _ -> false       
        else 
            false

    let isArithmeticStackMemoryWith8BitRegister(instruction: IntelInstruction, opCode: Opcode) =
        if instruction.Info.Opcode = opCode then
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regStackValue, _, _, opSize), OprReg regValue) 
                    when (isStackRegister(regStackValue) || opSize = 8<rt>) && is8BitRegister(regValue)  -> true
                | _ -> false
            | _ -> false       
        else 
            false

    let isMoveImmutableToStack(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.MOV
        | Opcode.MOVAPS
        | Opcode.MOVDQA ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some reg, scale, Some disposition, opSize), OprImm _) -> true
                | _ -> false
            | _ -> false
        | _ -> false  

    let isMoveDataToAVXRegister(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.MOVAPS
        | Opcode.MOVDQA ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprReg regValue, OprMem _) when isAVXRegister(regValue) -> true
                | _ -> false
            | _ -> false
        | _ -> false  
    
    let isJumpToPreviousInstruction(instruction: IntelInstruction) =
        if instruction.Info.Opcode = Opcode.JB then
            true
        elif instruction.Info.Opcode = Opcode.JMPNear then
             match instruction.Info.Operands with
             | OneOperand(OprDirAddr(Relative offset)) when offset < 0L -> true
             | _ -> false
        else
            false

    let isCallToFunctionWithDecryptionOperation(instruction: IntelInstruction, functionAddreses: UInt64 array) =
        match instruction.Info.Opcode with
        | Opcode.CALLNear ->
            match instruction.Info.Operands with
            | OneOperand(OprDirAddr(Relative offset)) ->
                let address = uint64(int64 instruction.Address + offset)
                if functionAddreses |> Array.contains address then true
                else false
            | _ -> false
        | _ -> false

    let isCallToExtraneousFunction(instruction: IntelInstruction, functionAddreses: UInt64 array) =
        match instruction.Info.Opcode with
        | Opcode.CALLFar -> true
        | Opcode.CALLNear -> not(isCallToFunctionWithDecryptionOperation(instruction, functionAddreses))
        | _ -> false

    let isSetByteRegisterInStackHeuristic(instruction: IntelInstruction) =  
        match instruction.Info.Opcode with
        | Opcode.MOV ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                match (firstOp, secondOp) with
                | (OprMem (Some regValue, scale, Some disposition, opSize), OprReg opReg) 
                    when isStackRegister(regValue) && (_registers8bit.Contains(opReg)) -> true
                | _ -> false
            | _ -> false
        | _ -> false