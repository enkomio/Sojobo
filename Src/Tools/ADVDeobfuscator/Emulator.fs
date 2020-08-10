namespace ES.ADVDeobfuscator

open System
open System.IO
open System.Reflection
open System.Text
open System.Collections.Generic
open ES.Sojobo.Windows
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open ES.ADVDeobfuscator.Entities
open ES.Sojobo.MemoryUtility
open ES.Fslog
open ES.Sojobo

type Emulator(fileName: String, func: Function, binScanner: BinaryScanner, maxInstructionCount: Int32, logProvider: ILogProvider) =    
    let _savedRegisters = new Dictionary<String, EmulatedValue>()
    
    let _logger =
        log "Emulator"
        |> verbose "EmulateFrom" "Start emulating function 0x{0} from 0x{1} to 0x{2}"
        |> info "ExtractedString" "Extracted string from instruction at 0x{0}: 0x{1}:{2}"
        |> verbose "TraceSaved" "Execution trace saved at: {0}"
        |> error "EmulationError" "Emulation error: {0}"
        |> error "NoStringFound" "No valid string was extracted from memory 0x{0}"
        |> error "NoStringExtracted" "No string were extracted during emulation"
        |> warning "MaxCountReached" "Reached threshold {0} for emulated instructions. Try to increase this value with --instructions argument"
        |> buildAndAdd(logProvider)

    let getVolatileRegisters() =
        // See: https://docs.microsoft.com/en-us/cpp/build/x64-software-conventions?view=vs-2019
        if binScanner.Functions.[0].Architecture = Arch.X86 then 
            [
                Register.EAX.ToString(); Register.ECX.ToString(); Register.EDX.ToString()                
            ]
        else 
            [
                Register.RAX.ToString(); Register.RCX.ToString(); Register.RDX.ToString()
                Register.R8.ToString(); Register.R9.ToString(); Register.R10.ToString()
                Register.R11.ToString();
            ]

    let getRegisters() =
        if binScanner.Functions.Length > 0 then
            if binScanner.Functions.[0].Architecture = Arch.X86 then 
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
        else
            List.empty

    let isOperandSafeToEmulate(operand: Operand) =
        match operand with
        | OprMem (Some regValue, scale, Some disposition, opSize) -> 
            regValue = Register.RBP || 
            regValue = Register.RSP || 
            _savedRegisters.ContainsKey(regValue.ToString())
        | OprReg _ -> true 
        | OprImm _ -> true
        | _ -> false

    let isInstructionSafeToEmulate(instruction: IntelInstruction) =
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
                isOperandSafeToEmulate(op)
            | NoOperand -> true
            | _ -> false
        | _ -> false

    let adjustStartAddress(func: Function, trace: ObfuscationTrace) =
        // this function will emulate more instruction that the one identified by the deobfuscator
        // This allows to set to a valid value potential registers used in the deobfuscation
        let mutable startAddress = trace.StartAddress
        match func.TryGetInstruction(trace.StartAddress) with
        | Some instruction ->
            // go back until I found a branch or a MOV with a reference not the stack
            let mutable curInstruction = instruction
            let mutable completed = false
            while not completed do                
                let prevInstruction = func.GetPreviousInstruction(curInstruction)
                completed <- 
                    prevInstruction.Address = curInstruction.Address ||
                    not <| isInstructionSafeToEmulate(prevInstruction)                    
                
                startAddress <- curInstruction.Address
                curInstruction <- prevInstruction
        | None -> ()

        // return result
        {trace with StartAddress = startAddress}

    let adjustEndAddress(func: Function, trace: ObfuscationTrace) =
        // this function will emulate more instruction that the one identified by the deobfuscator
        // This allows to set to a valid value potential registers used in the deobfuscation
        let mutable endAddress = trace.EndAddress
        func.Instructions
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

    let getIncrementRegisterName(func: Function, trace: ObfuscationTrace) =
        func.Instructions
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

    let saveTrace(trace: ObfuscationTrace, traceContent: String) =
        let traceDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "Trace")
        Directory.CreateDirectory(traceDirectory) |> ignore
        let traceFile = Path.Combine(traceDirectory, String.Format("0x{0}-0x{1}.txt", trace.StartAddress.ToString("X"), trace.EndAddress.ToString("X")))
        File.WriteAllText(traceFile, traceContent)
        _logger?TraceSaved(Path.GetFileName(traceFile)) |> ignore

    let saveRegisters(proc: IProcessContainer) =
        getRegisters()
        |> List.iter(fun reg -> _savedRegisters.[reg] <- proc.Cpu.GetRegister(reg))

    let trySetRegistryValue(regName: String, sandbox: WindowsSandbox, trace: ObfuscationTrace) =
        let proc = sandbox.GetRunningProcess()
        let mutable curInstruction = proc.GetInstruction(trace.StartAddress) :?> IntelInstruction
        let mutable instructionFound = false
        while not instructionFound do
            let tmpInstruction = func.GetPreviousInstruction(curInstruction)
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

    let setRegisters(sandbox: WindowsSandbox, trace: ObfuscationTrace) =        
        getRegisters()
        |> List.iter(fun regName ->
            match _savedRegisters.TryGetValue(regName) with
            | (true, regValue) -> sandbox.GetRunningProcess().Cpu.SetRegister(regValue)
            | _ -> trySetRegistryValue(regName, sandbox, trace)
        )
        
    let resetVolatileRegisterIfNecessary(sandbox: WindowsSandbox, func: Function, trace: ObfuscationTrace) = 
        // if there is a call, the volatile registers must be reset
        func.Instructions
        |> Array.filter(fun instruction -> instruction.Address < trace.StartAddress)
        |> Array.exists(fun instruction -> instruction.IsCall())
        |> function
            | false -> ()
            | true ->
                let zeroValue = if func.Architecture = Arch.X64 then createUInt64(0UL) else createUInt32(0u)
                let cpu = sandbox.GetRunningProcess().Cpu
                getVolatileRegisters()
                |> List.iter(fun regName ->
                    let register = {zeroValue with Name = regName; IsTemp = false}
                    cpu.SetRegister(register)
                )

    member this.Emulate(trace: ObfuscationTrace) =
        let settings = {
            InitializeEnvironment = false
            SaveSnapshotOnException = false
            CacheInstructions = true
        }

        // create sandbox
        let pointerSize = if func.Architecture = Arch.X64 then 64 else 32
        let sandbox = new WindowsSandbox(pointerSize, settings)
        sandbox.Load(fileName)
        let proc = sandbox.GetRunningProcess()

        // configure sandbox (do it for both x86 and x64)
        let adjustedTrace = adjustEndAddress(func, adjustStartAddress(func, trace))

        // set the savedvalues from registers. This is useful to manage compiler
        // optimization where a register value is set in a trace and reused in other trace
        setRegisters(sandbox, trace)
        resetVolatileRegisterIfNecessary(sandbox, func, trace)

        if proc.PointerSize = 32 then
            let eip = {createUInt32(uint32 adjustedTrace.StartAddress) with Name = Register.EIP.ToString(); IsTemp = false}        
            proc.Cpu.SetRegister(eip)
        else            
            let rip = {createUInt64(adjustedTrace.StartAddress) with Name = Register.RIP.ToString(); IsTemp = false}        
            proc.Cpu.SetRegister(rip)

        // setup trace content log file
        let emulatedTrace = new StringBuilder()
        let traceHeader = new StringBuilder()
        _logger?EmulateFrom(func.Address.ToString("X"), adjustedTrace.StartAddress.ToString("X"), adjustedTrace.EndAddress.ToString("X"))        
                
        traceHeader.AppendLine("-=[ Trace Info ]=-") |> ignore
        traceHeader.AppendFormat("Architecture: {0}", func.Architecture).AppendLine() |> ignore
        traceHeader.AppendFormat("Function start: 0x{0}", func.Address.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("Original start: 0x{0}", trace.StartAddress.ToString("X")).AppendLine() |> ignore
        if adjustedTrace.StartAddress <> trace.StartAddress then        
            traceHeader.AppendFormat("Adjusted start: 0x{0}", adjustedTrace.StartAddress.ToString("X")).AppendLine() |> ignore                
        traceHeader.AppendFormat("Deobfuscate address: 0x{0}", trace.DeobfuscateOperationAddress.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("End address: 0x{0}", trace.EndAddress.ToString("X")).AppendLine() |> ignore
        if adjustedTrace.EndAddress <> trace.EndAddress then        
            traceHeader.AppendFormat("Adjusted end: 0x{0}", adjustedTrace.EndAddress.ToString("X")).AppendLine() |> ignore                
        
        // try to identify the register used as index increment
        getIncrementRegisterName(func, trace)
        |> Option.iter(fun (size, regName) ->
            traceHeader.AppendFormat("Using increment register: {0}", regName).AppendLine() |> ignore            
            let tmpReg = if size = 32<B2R2.rt> then createUInt32(1u) else createUInt64(1UL)
            let reg = {tmpReg with Name = regName.ToUpperInvariant(); IsTemp = false}        
            proc.Cpu.SetRegister(reg)  
        )

        emulatedTrace.AppendLine().AppendLine("-=[ Registers ]=-") |> ignore
        getRegisters()
        |> List.iter(fun register ->
            let address = proc.Cpu.GetRegister(register).As<UInt64>()
            let info =
                if proc.Memory.IsAddressMapped(address) && not(String.IsNullOrWhiteSpace(proc.Memory.GetMemoryRegion(address).Info))
                then String.Format("; {0} ", proc.Memory.GetMemoryRegion(address).Info)
                else String.Empty
            emulatedTrace.AppendFormat("{0}=0x{1} {2}", register, address.ToString("X"), info).AppendLine() |> ignore
        )
        emulatedTrace.AppendLine().AppendLine("-=[ Instructions ]=-") |> ignore

        // setup handler and run emulation
        let mutable stringBufferAddressStart: UInt64 = 0UL
        let mutable stringBufferAddressEnd: UInt64 = 0UL
        let mutable instructionCount = 0
        let callStack = new Stack<UInt64>()

        proc.Memory.MemoryAccess.Add(fun memOp ->                    
            match memOp with
            | Write (address, _) -> 
                if stringBufferAddressEnd + 1UL = address then
                    // increase only
                    stringBufferAddressEnd <- address

                if stringBufferAddressStart = 0UL then
                    let instruction = proc.GetInstruction() :?> IntelInstruction
                    let flags = binScanner.AnalyzeInstruction(func, instruction)
                    if setResultOperationFound(func, instruction, adjustedTrace, flags) then
                        traceHeader
                            .AppendFormat("Write deobfuscation result instruction at 0x{0}", instruction.Address.ToString("X"))
                            .AppendLine()
                            .AppendFormat("String stored at: 0x{0}", address.ToString("X"))
                            .AppendLine() |> ignore
                        stringBufferAddressStart <- address
                        stringBufferAddressEnd <- address
            | _ -> ()
        )

        sandbox.BeforeEmulation.Add(fun proc ->         
            let instruction = proc.GetInstruction() :?> IntelInstruction
            if instruction.Info.Opcode = Opcode.CALLNear then
                // ensure that is not a call to an extraneous function  
                let flags = binScanner.AnalyzeInstruction(func, instruction)
                if terminatingOperationFound(flags) then
                    sandbox.Stop()
                else
                    callStack.Push(proc.ProgramCounter.As<UInt64>())
            
            emulatedTrace.AppendLine(ES.Sojobo.Utility32.disassemble(proc, proc.GetInstruction())) |> ignore
        )
        
        sandbox.AfterEmulation.Add(fun proc ->         
            instructionCount <- instructionCount + 1
            let instruction = proc.GetInstruction() :?> IntelInstruction            
            if instruction.Info.Opcode = Opcode.RETNear then
                callStack.Pop() |> ignore
            elif callStack.Count = 0 && proc.ProgramCounter.As<UInt64>() > adjustedTrace.EndAddress then
                sandbox.Stop()
            elif instructionCount > maxInstructionCount then
                _logger?MaxCountReached(maxInstructionCount)
                sandbox.Stop()
        )

        let mutable stringExtracted = false
        try 
            sandbox.Run()

            // save the register values
            saveRegisters(proc)

            // read the string
            if stringBufferAddressStart > 0UL then
                let mutable decodedString = 
                    let tmp = readUnicodeString(proc.Memory, stringBufferAddressStart)
                    if String.IsNullOrEmpty(tmp) then readAsciiString(proc.Memory, stringBufferAddressStart)
                    else tmp

                // check if I read soemthing weird
                let addressLength = int32(stringBufferAddressEnd - stringBufferAddressStart) + 1
                if addressLength > 1 && addressLength < decodedString.Length then
                    decodedString <- decodedString.Substring(0, addressLength)

                if not(String.IsNullOrEmpty(decodedString)) then
                    _logger?ExtractedString(trace.DeobfuscateOperationAddress.ToString("X"), stringBufferAddressStart.ToString("X"), decodedString)
                    stringExtracted <- true
                    traceHeader.AppendFormat("Extracted string: {0}", decodedString).AppendLine() |> ignore
                else
                    _logger?NoStringFound(stringBufferAddressEnd.ToString("X"))
            else
                _logger?NoStringExtracted()
        with e -> 
            _logger?EmulationError(e.Message)
            emulatedTrace.AppendLine().AppendLine("-=[ Error ]=-").AppendLine(e.ToString()) |> ignore        

        // finally save the trace
        saveTrace(adjustedTrace, (traceHeader.ToString()) + (emulatedTrace.ToString()))
        stringExtracted
