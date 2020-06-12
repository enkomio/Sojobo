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

type Emulator(fileName: String, binScanner: BinaryScanner, logProvider: ILogProvider) =
    let _logger =
        log "Emulator"
        |> verbose "EmulateFrom" "Start emulating code from 0x{0} (original 0x{1}) - 0x{2}"
        |> info "ExtractedString" "Extracted string from instruction at 0x{0}: 0x{1}:{2}"
        |> verbose "TraceSaved" "Execution trace saved at: {0}"
        |> error "EmulationError" "Emulation error: {0}"
        |> error "NoStringFound" "No valid string was extracted from memory 0x{0}"
        |> buildAndAdd(logProvider)

    let isOperandSafeToEmulate(operand: Operand) =
        match operand with
        | OprMem (Some regValue, scale, Some disposition, opSize) -> regValue = Register.RBP || regValue = Register.RSP
        | OprReg _ -> true 
        | OprImm _ -> true
        | _ -> false

    let isInstructionSafeToEmulate(instruction: IntelInstruction) =
        match instruction.Info.Opcode with
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
                    not <| isInstructionSafeToEmulate(prevInstruction) ||
                    prevInstruction.Address = curInstruction.Address
                
                startAddress <- curInstruction.Address
                curInstruction <- prevInstruction
        | None -> ()

        // return result
        {trace with StartAddress = startAddress}

    let getIncrementRegisterName(func: Function, trace: ObfuscationTrace) =
        func.Instructions
        |> Array.filter(fun instruction -> instruction.Address >= trace.StartAddress && instruction.Address <= trace.EndAddress)
        |> Array.map(fun instruction ->
            if instruction.Info.Opcode = Opcode.ADD then
                match instruction.Info.Operands with
                | TwoOperands (OprReg opReg1, OprReg opReg2) when opReg1 <> opReg2 -> opReg2.ToString()
                | _ -> String.Empty
            else
                String.Empty
        )
        |> Array.filter(String.IsNullOrWhiteSpace >> not)
        |> Array.tryHead

    let saveTrace(trace: ObfuscationTrace, traceContent: String) =
        let traceDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "Trace")
        Directory.CreateDirectory(traceDirectory) |> ignore
        let traceFile = Path.Combine(traceDirectory, String.Format("0x{0}-0x{1}.txt", trace.StartAddress.ToString("X"), trace.EndAddress.ToString("X")))
        File.WriteAllText(traceFile, traceContent)
        _logger?TraceSaved(Path.GetFileName(traceFile))

    member this.Emulate(func: Function, trace: ObfuscationTrace) =
        let settings = {
            InitializeEnvironment = false
            SaveSnapshotOnException = false
            CacheInstructions = true
        }
        let sandbox = new WindowsSandbox(64, settings)
        sandbox.Load(fileName)
        let proc = sandbox.GetRunningProcess()

        // try to identify the register used as index increment
        getIncrementRegisterName(func, trace)
        |> Option.iter(fun regName ->
            let reg = {createUInt64(1UL) with Name = regName.ToUpperInvariant(); IsTemp = false}        
            proc.Cpu.SetRegister(reg)  
        )

        // configure sandbox (do it for both x86 and x64)
        let adjustedTrace = adjustStartAddress(func, trace)
        if proc.PointerSize = 32 then
            let eip = {createUInt64(adjustedTrace.StartAddress) with Name = "EIP"; IsTemp = false}        
            proc.Cpu.SetRegister(eip)
        else            
            let rip = {createUInt64(adjustedTrace.StartAddress) with Name = "RIP"; IsTemp = false}        
            proc.Cpu.SetRegister(rip)

        // setup trace content log file
        let emulatedTrace = new StringBuilder()
        let traceHeader = new StringBuilder()
        
        traceHeader.AppendLine("-=[ Trace Info ]=-") |> ignore
        traceHeader.AppendFormat("Function start: 0x{0}", func.Address.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("Original start: 0x{0}", trace.StartAddress.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("Adjusted start: 0x{0}", adjustedTrace.StartAddress.ToString("X")).AppendLine() |> ignore        
        traceHeader.AppendFormat("Deobfuscate address: 0x{0}", trace.DeobfuscateOperationAddress.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("End address: 0x{0}", trace.EndAddress.ToString("X")).AppendLine() |> ignore

        emulatedTrace.AppendLine().AppendLine("-=[ Registers ]=-") |> ignore
        if proc.PointerSize = 32 then ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"; "ESP"; "EBP"; "EIP"]
        else ["RAX"; "RBX"; "RCX"; "RDX"; "RSI"; "RDI"; "RSP"; "RBP"; "RIP"; "R8"; "R9"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15"]
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
                    if encryptionOperationFound(flags) then
                        traceHeader
                            .AppendFormat(
                                "Effective deobfuscation operation at 0x{0}, buffer at: 0x{1}", 
                                instruction.Address.ToString("X"), 
                                address.ToString("X")
                            ).AppendLine() |> ignore
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
            elif callStack.Count = 0 && proc.ProgramCounter.As<UInt64>() > trace.EndAddress then
                sandbox.Stop()
            elif instructionCount > 500 then
                sandbox.Stop()
        )

        _logger?EmulateFrom(adjustedTrace.StartAddress.ToString("X"), trace.StartAddress.ToString("X"), trace.EndAddress.ToString("X"))        
        try sandbox.Run()
        with e -> 
            _logger?EmulationError(e.Message)
            emulatedTrace.AppendLine().AppendLine("-=[ Error ]=-").AppendLine(e.ToString()) |> ignore        

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

            if not(String.IsNullOrWhiteSpace(decodedString)) then
                _logger?ExtractedString(trace.DeobfuscateOperationAddress.ToString("X"), stringBufferAddressStart.ToString("X"), decodedString)
                traceHeader.AppendFormat("Extracted string: {0}", decodedString).AppendLine() |> ignore
            else
                _logger?NoStringFound(stringBufferAddressEnd.ToString("X"))

        // finally save the trace
        saveTrace(adjustedTrace, (traceHeader.ToString()) + (emulatedTrace.ToString()))
