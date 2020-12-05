namespace ES.ADVDeobfuscator

open System
open System.Collections.Generic
open Entities
open B2R2
open B2R2.FrontEnd.Intel
open ES.Sojobo.Model
open ES.Sojobo
open ES.Sojobo.Windows

[<RequireQualifiedAccess>]
module EmulatorHelper =
    let private getVolatileRegisters(func: Function) =
        // See: https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?view=vs-2019
        if func.Architecture = Arch.X86 then 
            [
                Register.EAX.ToString(); Register.ECX.ToString(); Register.EDX.ToString()                
            ]
        else 
            [
                Register.RAX.ToString(); Register.RCX.ToString(); Register.RDX.ToString()
                Register.R8.ToString(); Register.R9.ToString(); Register.R10.ToString()
                Register.R11.ToString();
            ]

    let getRegisters(func: Function) =
        if func.Architecture = Arch.X86 then 
            [
                Register.EAX.ToString(); Register.EBX.ToString(); Register.ECX.ToString()
                Register.EDX.ToString(); Register.ESI.ToString(); Register.EDI.ToString()
                Register.ESP.ToString(); Register.EBP.ToString()
            ]
        else 
            [
                Register.RAX.ToString(); Register.RBX.ToString(); Register.RCX.ToString()
                Register.RDX.ToString(); Register.RSI.ToString(); Register.RDI.ToString()
                Register.RSP.ToString(); Register.RBP.ToString(); Register.R15.ToString()
                Register.R8.ToString(); Register.R9.ToString(); Register.R10.ToString()
                Register.R11.ToString(); Register.R12.ToString(); Register.R13.ToString()
                Register.R14.ToString()
            ]

    let private isOperandSafeToEmulate(operand: Operand)=
        match operand with
        | OprMem (Some regValue, scale, Some disposition, opSize) -> 
            regValue = Register.RBP || regValue = Register.EBP || 
            regValue = Register.RSP || regValue = Register.ESP
        | OprReg _ -> true 
        | OprImm _ -> true
        | _ -> false

    let private isInstructionSafeToEmulate(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
        | Opcode.NOP -> true
        | Opcode.MOV ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) ->
                [firstOp; secondOp]
                |> List.forall(isOperandSafeToEmulate)
            | _ -> false
        | Opcode.ADD
        | Opcode.SUB
        | Opcode.POP
        | Opcode.PUSH
        | Opcode.LEA
        | Opcode.XOR ->
            match instruction.Info.Operands with
            | TwoOperands (firstOp, secondOp) -> 
                [firstOp; secondOp]
                |> List.forall(isOperandSafeToEmulate)
            | OneOperand op -> 
                isOperandSafeToEmulate op
            | NoOperand -> true
            | _ -> false
        | _ -> false

    let adjustStartAddress(trace: ObfuscationTrace, savedRegisters: Dictionary<String, EmulatedValue>) =
        // this function will emulate more instruction that the one identified by the deobfuscator
        // This allows to set to a valid value potential registers used in the deobfuscation
        let mutable startAddress = trace.StartAddress
        match trace.Function.TryGetInstruction(trace.StartAddress) with
        | Some instruction ->
            // go back until I found a branch or a MOV with a reference not the stack
            let mutable curInstruction = instruction
            let mutable completed = false
            while not completed do                
                let prevInstruction = trace.Function.GetPreviousInstruction(curInstruction)
                completed <- 
                    prevInstruction.Address = curInstruction.Address ||
                    not <| isInstructionSafeToEmulate(prevInstruction)                    
                
                startAddress <- curInstruction.Address
                curInstruction <- prevInstruction
        | None -> ()

        // return result
        {trace with StartAddress = startAddress}

    let private isVolativeRegister(reg: Register) =
        [
            Register.RAX; Register.EAX 
            Register.RCX; Register.ECX
            Register.RDX; Register.EDX
            Register.R8; Register.R9; Register.R10; Register.R11
        ]
        |> List.contains reg

    let private tryGetRegisterValue(targetReg: Register, trace: ObfuscationTrace) =
        let mutable pushPopStep = false
        let mutable savedRegister: Register option = None
        let mutable result: Int64 option = None
        let mutable stopInstructionFound = false
        let isVolatile = isVolativeRegister targetReg
        
        let extendRegister =
            if trace.Function.Architecture = Arch.X86 then Register.extendRegister32
            else Register.extendRegister64
        
        trace.Function.Instructions
        |> Array.filter(fun instruction -> instruction.Address < trace.DeobfuscateOperationAddress)
        |> Array.rev
        |> Array.iter(fun instruction ->
            if not stopInstructionFound then
                match instruction.Info.Opcode with
                | Opcode.CALLFar
                | Opcode.CALLNear -> 
                    // when a call is found volatile registers are considered not more in
                    // a know state
                    stopInstructionFound <- isVolatile
                | Opcode.MOV ->
                    // move an hard-coded value to the register
                    match instruction.Info.Operands with
                    | TwoOperands (OprReg destReg, secondOpr) -> 
                        if extendRegister destReg = targetReg then
                            match secondOpr with
                            | OprImm oprImm ->
                                // register value found
                                result <- Some oprImm
                            | _ ->
                                // another kind of initialization is performed,
                                // unable to get the real value
                                stopInstructionFound <- true
                    | _ -> ()
                | Opcode.POP ->
                    // identify push/pop initialization
                    match instruction.Info.Operands with
                    | OneOperand(OprReg reg) ->
                        if extendRegister reg = targetReg then
                            savedRegister <- Some reg
                            pushPopStep <- true
                        else
                            pushPopStep <- false
                    | _ -> ()
                | Opcode.PUSH ->
                    // identify push/pop initialization
                    if pushPopStep then
                        match instruction.Info.Operands with
                        | OneOperand(OprImm oprImm) when savedRegister.IsSome ->
                            result <- Some oprImm
                        | _ -> 
                            pushPopStep <- false
                | _ -> ()
        )
        result

    let getInitRegistersValue(trace: ObfuscationTrace) =
        let extendRegister =
            if trace.Function.Architecture = Arch.X86 then Register.extendRegister32
            else Register.extendRegister64

        trace.Function.Instructions
        |> Array.filter(fun instruction -> instruction.Address >= trace.StartAddress && instruction.Address <= trace.EndAddress)
        |> Array.choose(fun instruction ->
            match instruction.Info.Operands with
            | TwoOperands(_, OprReg oprReg) -> Some(extendRegister oprReg)
            | _ -> None
        )
        |> Array.distinct
        |> Array.choose(fun reg -> 
            match tryGetRegisterValue(reg, trace) with
            | Some value -> Some(reg, value)
            | _ -> None
        )

    let tryGetIncrementRegisterName(trace: ObfuscationTrace) =
        trace.Function.Instructions
        |> Array.filter(fun instruction -> instruction.Address >= trace.StartAddress && instruction.Address <= trace.EndAddress)
        |> Array.choose(fun instruction ->
            if instruction.Info.Opcode = Opcode.ADD then
                match instruction.Info.Operands with
                | TwoOperands (OprReg opReg1, OprReg opReg2) when opReg1 <> opReg2 -> Some (Register.toRegType(opReg2), opReg2.ToString())
                | _ -> None
            else
                None
        )
        |> Array.tryHead

    (*
    For XOR obfuscation teo types of key initialization where found, push imm/pop regKey
    or mov regKey, imm. This function tries to identfy these cases
    *)
    let tryGetObfuscationKey(trace: ObfuscationTrace) =
        let mutable result: (Register * EmulatedValue) option = None
        let mutable _registerKey: Register option = None

        let createValue(register: Register, value: Int64) =
            match Register.toRegType register with
            | 8<rt> -> createByte(byte value)
            | 16<rt> -> createUInt16(uint16 value)
            | 32<rt> -> createUInt32(uint32 value)
            | 64<rt> -> createUInt64(uint64 value)
            | _ -> failwith "Unable to create value"

        let extendFunction =
            if trace.Function.Architecture = Arch.X86 then Register.extendRegister32
            else Register.extendRegister64

        let deobfuscationInstruction = 
            trace.Function.Instructions
            |> Array.find(fun instruction -> instruction.Address = trace.DeobfuscateOperationAddress)
            
        match deobfuscationInstruction.Info.Opcode with
        | Opcode.XOR -> 
            match deobfuscationInstruction.Info.Operands with
            | TwoOperands (_, OprReg reg) -> 
                let keyReg = extendFunction reg
                let mutable pushPopKey = false

                // analyze all instructions from end to start to identify a possible key value
                trace.Function.Instructions
                |> Array.filter(fun instruction -> instruction.Address < trace.EndAddress)
                |> Array.rev
                |> Array.iter(fun instruction ->     
                    match result  with
                    | None ->
                        match instruction.Info.Opcode with
                        | Opcode.MOV ->
                            // move an hard-coded value to the register
                            match instruction.Info.Operands with
                            | TwoOperands (OprReg destReg, OprImm oprImm) -> 
                                if extendFunction destReg = keyReg then
                                    // register value found
                                    let extendedRegister = extendFunction(reg)
                                    result <- Some(extendedRegister, createValue(extendedRegister, oprImm))
                            | _ -> ()
                        | Opcode.POP ->
                            // identify push/pop initialization
                            match instruction.Info.Operands with
                            | OneOperand(OprReg reg) ->
                                if extendFunction reg = keyReg then
                                    _registerKey <- Some reg
                                    pushPopKey <- true
                                else
                                    pushPopKey <- false
                            | _ -> ()
                        | Opcode.PUSH ->
                            // identify push/pop initialization
                            if pushPopKey then
                                match instruction.Info.Operands with
                                | OneOperand(OprImm oprImm) when _registerKey.IsSome ->
                                    let extendedRegister = extendFunction(reg)
                                    result <- Some(extendedRegister, createValue(_registerKey.Value, oprImm))
                                | _ -> 
                                    pushPopKey <- false
                        | _ -> ()
                    | _ -> ()
                )
            | _ -> ()
        | _ -> ()

        result

    let adjustEndAddress(trace: ObfuscationTrace) =
        // this function will emulate more instruction that the one identified by the deobfuscator
        // This allows to set to a valid value potential registers used in the deobfuscation
        let mutable endAddress = trace.EndAddress
        trace.Function.Instructions
        |> Array.filter(fun instr -> 
            instr.IsCondBranch() && 
            uint64 instr.Address > trace.StartAddress &&
            uint64 instr.Address < endAddress
        )
        |> Array.iter(fun instr ->
            match instr.Info.Operands with            
            | OneOperand (OprDirAddr (Relative offset)) when instr.Address + uint64 offset > endAddress ->
                endAddress <- instr.Address + uint64 offset
            | _ -> ()
        )

        // return result
        {trace with EndAddress = endAddress}    

    let private trySetRegistryValue(regName: String, sandbox: WindowsSandbox, trace: ObfuscationTrace) =
        let proc = sandbox.GetRunningProcess()
        let mutable curInstruction = proc.GetInstruction(trace.StartAddress) :?> IntelInstruction
        let mutable instructionFound = false
        while not instructionFound do
            let tmpInstruction = trace.Function.GetPreviousInstruction(curInstruction)
            if tmpInstruction.Address = curInstruction.Address then
                instructionFound <- true
            else
                curInstruction <- tmpInstruction
                if [Opcode.MOV; Opcode.LEA] |> List.contains curInstruction.Info.Opcode then
                    match curInstruction.Info.Operands with
                    | TwoOperands(OprReg reg, OprMem (Some register, None, Some disp, _)) when 
                        Opcode.LEA = curInstruction.Info.Opcode &&  
                        register <> Register.EBP &&
                        register <> Register.RBP
                            -> Some reg
                    | TwoOperands(OprReg reg, OprImm imm) -> Some reg
                    | _ -> None
                    |> Option.iter(fun instrReg ->
                        if instrReg.ToString().Equals(regName, StringComparison.OrdinalIgnoreCase) then
                            // register found, try to set its value 
                            // by emulating the single instruction                            
                            instructionFound <- true                            
                            sandbox.EmulateInstruction(curInstruction)
                    )  

    let private setRegisters(sandbox: WindowsSandbox, trace: ObfuscationTrace, savedRegisters: Dictionary<String, EmulatedValue>) =        
        getRegisters(trace.Function)
        |> List.iter(fun regName ->
            match savedRegisters.TryGetValue(regName) with
            | (true, regValue) -> sandbox.GetRunningProcess().Cpu.SetRegister(regValue)
            | _ -> trySetRegistryValue(regName, sandbox, trace)
        )        