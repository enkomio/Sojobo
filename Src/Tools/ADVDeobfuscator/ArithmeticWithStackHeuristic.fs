namespace ES.ADVDeobfuscator

open System
open System.Collections.Generic
open Entities
open B2R2.FrontEnd.Intel
open ES.Fslog

type ArithmeticWithStackHeuristic(functionAddresses: UInt64 array, logProvider: ILogProvider) =
    inherit HeuristicBase()
    let _jumpInstructionAddresses = new Dictionary<IntelInstruction, UInt64>()
    let mutable _analyzedInstructionCount = 0
    let mutable _deobfuscationAddress = 0UL
    let mutable _startAddress = 0UL
    let mutable _endAddress = 0UL
    let mutable _flags = DeobfuscationFlag.Empty

    let _logger =
        log "ArithmeticWithStackHeuristic"
        |> warning "InconsistentAddress" "Address 0x{0} is inconsistent. Change start address to the next one: 0x{1}"
        |> buildAndAdd(logProvider)

    let resetState() =
        _deobfuscationAddress <- 0UL
        _startAddress <- 0UL
        _endAddress <- 0UL
        _flags <- DeobfuscationFlag.Empty
        _analyzedInstructionCount <- 0
        _jumpInstructionAddresses.Clear()

    let checkStartDeobfuscation(instruction: IntelInstruction) =
        if _startAddress = 0UL then
            if HeuristicHelper.isMoveImmutableToStack(instruction) then
                _flags <- _flags ||| DeobfuscationFlag.MovToStack
                _startAddress <- instruction.Address
            elif HeuristicHelper.isMoveDataToAVXRegister(instruction) then
                _flags <- _flags ||| DeobfuscationFlag.MovToAVXRegister
                _startAddress <- instruction.Address

    let checkDeobfuscationOperation(instruction: IntelInstruction) =
        if _deobfuscationAddress = 0UL && _startAddress > 0UL then
            if HeuristicHelper.isArithmeticStackMemoryWith8BitRegister(instruction, Opcode.XOR) then
                _flags <- _flags ||| DeobfuscationFlag.XorWith8BitRegister
                _deobfuscationAddress <- instruction.Address
            elif HeuristicHelper.isArithmeticStackMemoryWith8BitRegister(instruction, Opcode.SUB) then
                _flags <- _flags ||| DeobfuscationFlag.SubWithImmediateOrRegister
                _deobfuscationAddress <- instruction.Address
            elif HeuristicHelper.isArithmeticStackMemoryWith8BitRegister(instruction, Opcode.ADD) then
                _flags <- _flags ||| DeobfuscationFlag.AddWithImmediateOrRegister
                _deobfuscationAddress <- instruction.Address
            elif HeuristicHelper.isCallToFunctionWithDecryptionOperation(instruction, functionAddresses) then
                _flags <- _flags ||| DeobfuscationFlag.CallToFunctionDecrypt
                _deobfuscationAddress <- instruction.Address

    let checkEndOfDeobfuscation(instruction: IntelInstruction) =
        if _endAddress = 0UL && _startAddress > 0UL && _deobfuscationAddress > 0UL then
            if HeuristicHelper.isJumpToPreviousInstruction(instruction) then
                _flags <- _flags ||| DeobfuscationFlag.JumpToPreviousAddress
                _endAddress <- instruction.Address
            elif HeuristicHelper.isSetByteRegisterInStackHeuristic(instruction) then
                _flags <- _flags ||| DeobfuscationFlag.SetByteRegisterInStack
                _endAddress <- instruction.Address

    let checkInvalidState(instruction: IntelInstruction) =
        if HeuristicHelper.isCallToExtraneousFunction(instruction, functionAddresses) then
            resetState()  
            
    let analyzeBrach(instruction: IntelInstruction) =
        if _startAddress > 0UL && instruction.IsBranch() then
            match instruction.Info.Operands with
            | OneOperand(OprDirAddr(Relative offset)) -> 
                let destination = uint64(int64 instruction.Address + offset)
                _jumpInstructionAddresses.Add(instruction, destination)
            | _ -> ()

    let checkForInconsistentValues() =
        _jumpInstructionAddresses
        |> Seq.iter(fun kv ->
            let (instruction, destination) = (kv.Key, kv.Value)
            // sometimes there are jump to outside the trace. Found in taurus_dump.bin sample
            if instruction.Address < _deobfuscationAddress && destination > _endAddress then
                let nextAddress = instruction.Address + uint64 instruction.Length
                _startAddress <- nextAddress
                _logger?InconsistentAddress(instruction.Address.ToString("X"), nextAddress.ToString("X"))
        )

    default this.Reset() =
        resetState()

    default this.AnalyzeInstruction(func: Function, instruction: IntelInstruction) =
        _analyzedInstructionCount <- _analyzedInstructionCount + 1
        if _analyzedInstructionCount > 50 then
            this.Reset()
        else
            analyzeBrach(instruction)
            checkStartDeobfuscation(instruction)
            checkDeobfuscationOperation(instruction)   
            checkEndOfDeobfuscation(instruction)
            checkInvalidState(instruction)

    default this.IsSatisfied() =
        _startAddress > 0UL && _deobfuscationAddress > 0UL && _endAddress > 0UL

    default this.DeobfuscationFound() =
        _deobfuscationAddress > 0UL

    default this.HasFlag(flag: DeobfuscationFlag) =
        _flags.HasFlag(flag)

    default this.GetTrace(func: Function) =
        checkForInconsistentValues()
        {
            StartAddress = _startAddress
            DeobfuscateOperationAddress = _deobfuscationAddress
            EndAddress = _endAddress
            Function = func
        }