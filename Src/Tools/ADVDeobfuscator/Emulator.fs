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
open B2R2

type Emulator(id: String, fileName: String, func: Function, heuristic: HeuristicAggregator, maxInstructionCount: Int32, logProvider: ILogProvider) =    
    let mutable _inExceptionHandler = false
    let _savedRegisters = new Dictionary<String, EmulatedValue>()
    let _deobfuscationFlags = [
        DeobfuscationFlag.AddWithImmediateOrRegister
        DeobfuscationFlag.SubWithImmediateOrRegister
        DeobfuscationFlag.XorWith8BitRegister
        DeobfuscationFlag.XorWithStack
    ]
    
    let _logger =
        log "Emulator"
        |> verbose "EmulateFrom" "Start emulating function 0x{0} from 0x{1} to 0x{2}"
        |> info "ExtractedString" "Extracted string from instruction at 0x{0} [0x{1} - 0x{2}]: {3}"
        |> verbose "TraceSaved" "Execution trace saved at: {0}"
        |> verbose "SetRegistryKey" "Set register {0} used for deobfuscation to: 0x{1}"
        |> error "EmulationError" "Faulty address: 0x{0}, Trace: {1}, Exception: {2}"        
        |> error "NoStringFound" "No valid string was extracted from memory 0x{0}. Trace: {1}"
        |> warning "MaxCountReached" "Reached threshold {0} for emulated instructions. Try to increase this value with --instructions argument"
        |> buildAndAdd(logProvider)     

    let saveTrace(trace: ObfuscationTrace, traceContent: String) =
        let traceDirectory = Path.Combine(Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), "Trace", id)
        Directory.CreateDirectory(traceDirectory) |> ignore
        let traceFile = Path.Combine(traceDirectory, String.Format("0x{0}-0x{1}.txt", trace.StartAddress.ToString("X"), trace.EndAddress.ToString("X")))
        File.WriteAllText(traceFile, traceContent)
        _logger?TraceSaved(Path.GetFileName(traceFile)) |> ignore 

    let emulationException(sandbox: ISandbox, error: ExecutionException) =  
        if _inExceptionHandler then
            false
        else
            match error with
            | MemoryAccessViolation e -> 
                match e.Error with
                | MemoryAccessionViolationError.MemoryNotMapped ->
                    _inExceptionHandler <- true
                    // just ignore the current instruction and go to the next one
                    let proc = sandbox.GetRunningProcess()
                    let instruction = proc.GetInstruction()
                    let (nextAddress, _) = instruction.GetNextInstrAddrs() |> Seq.head
                    let eip = proc.Cpu.GetRegister("EIP")
                    proc.Cpu.SetRegister({eip with Value = createValue(nextAddress).Value})            
                    _inExceptionHandler <- false
                | _ -> 
                    ()
                true
            | _ -> false

    member this.Emulate(trace: ObfuscationTrace) =
        // create sandbox
        let pointerSize = if func.Architecture = Arch.X64 then 64 else 32
        let sandbox = 
            new WindowsSandbox(
                pointerSize, 
                {
                    InitializeEnvironment = false
                    SaveSnapshotOnException = false
                    CacheInstructions = true
                }
            )
        sandbox.Load(fileName)
        let proc = sandbox.GetRunningProcess()

        // configure sandbox (do it for both x86 and x64)
        let adjustedTrace = EmulatorHelper.adjustStartAddress(trace, _savedRegisters)
        let adjustedTrace = EmulatorHelper.adjustEndAddress(adjustedTrace)

        // set start address
        if proc.PointerSize = 32 then
            let eip = {createUInt32(uint32 adjustedTrace.StartAddress) with Name = Register.EIP.ToString(); IsTemp = false}        
            proc.Cpu.SetRegister(eip)
        else            
            let rip = {createUInt64(adjustedTrace.StartAddress) with Name = Register.RIP.ToString(); IsTemp = false}        
            proc.Cpu.SetRegister(rip)

        // setup trace content log file
        let emulatedTrace = new StringBuilder()
        let traceHeader = new StringBuilder()
        _logger?EmulateFrom(func.StartAddress.ToString("X"), adjustedTrace.StartAddress.ToString("X"), adjustedTrace.EndAddress.ToString("X"))        
                
        traceHeader.AppendLine("-=[ Trace Info ]=-") |> ignore
        traceHeader.AppendFormat("Architecture: {0}", func.Architecture).AppendLine() |> ignore
        traceHeader.AppendFormat("Function start: 0x{0}", func.StartAddress.ToString("X")).AppendLine() |> ignore
        traceHeader.AppendFormat("Start address: 0x{0}", adjustedTrace.StartAddress.ToString("X")).AppendLine() |> ignore                
        traceHeader.AppendFormat("Deobfuscate address: 0x{0}", trace.DeobfuscateOperationAddress.ToString("X")).AppendLine() |> ignore        
        traceHeader.AppendFormat("End address: 0x{0}", adjustedTrace.EndAddress.ToString("X")).AppendLine() |> ignore                
        
        // try to initialize the used registers
        EmulatorHelper.getInitRegistersValue(trace)
        |> Array.iter(fun (regName, value) ->
            let register = proc.Cpu.GetRegister(regName.ToString()).SetValue(uint64 value)
            proc.Cpu.SetRegister(register)  
        )

        // try to identify the register used as index increment
        EmulatorHelper.tryGetIncrementRegisterName(trace)
        |> Option.iter(fun (size, regName) ->
            let tmpReg = if size = 32<B2R2.rt> then createUInt32(1u) else createUInt64(1UL)
            let reg = {tmpReg with Name = regName.ToUpperInvariant(); IsTemp = false}        
            proc.Cpu.SetRegister(reg)  
        )

        // for XOR obfuscation, try to identify the key
        EmulatorHelper.tryGetObfuscationKey(adjustedTrace)
        |> Option.iter(fun (register, value) ->
            let keyValue = (value.Value |> BitVector.toUInt64).ToString("X")
            _logger?SetRegistryKey(register, keyValue)
            let keyReg = {value with Name = register.ToString(); IsTemp = false}        
            proc.Cpu.SetRegister(keyReg)                
        )

        // write register to output
        emulatedTrace.AppendLine().AppendLine("-=[ Registers ]=-") |> ignore
        EmulatorHelper.getRegisters(func)
        |> List.iter(fun register ->
            let address = proc.Cpu.GetRegister(register).As<UInt64>()
            let info =
                match proc.Memory.GetMemoryRegion(address) with
                | Some region when not(String.IsNullOrWhiteSpace(region.Info)) -> String.Format("; {0} ", region.Info)
                | _ -> String.Empty
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
            | Write (address, res) -> 
                if stringBufferAddressEnd + 1UL = address then
                    // increase only
                    stringBufferAddressEnd <- address

                if stringBufferAddressStart = 0UL then
                    let instruction = proc.GetInstruction() :?> IntelInstruction
                    heuristic.AnalyzeInstruction(func, instruction)
                    if _deobfuscationFlags |> List.exists(heuristic.HasFlag) then
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
                callStack.Push(proc.ProgramCounter.As<UInt64>())   
            elif instruction.Info.Opcode = Opcode.CALLFar then
                sandbox.Stop()
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

        sandbox.EmulationException <- emulationException
        let mutable stringExtracted = false

        try 
            sandbox.Run()        
            // save the register values
            //saveRegisters(proc)

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
                    _logger?ExtractedString(
                        adjustedTrace.DeobfuscateOperationAddress.ToString("X"), 
                        adjustedTrace.StartAddress.ToString("X"), 
                        adjustedTrace.EndAddress.ToString("X"),
                        decodedString
                    )
                    stringExtracted <- true
                    traceHeader.AppendFormat("Extracted string: {0}", decodedString).AppendLine() |> ignore
                else
                    _logger?NoStringFound(stringBufferAddressEnd.ToString("X"), trace)
            else
                _logger?NoStringFound(stringBufferAddressEnd.ToString("X"), trace)

            saveTrace(adjustedTrace, (traceHeader.ToString()) + (emulatedTrace.ToString()))
        with e -> 
            saveTrace(adjustedTrace, (traceHeader.ToString()) + (emulatedTrace.ToString()))
            try
                let errorAddress = sandbox.GetRunningProcess().GetInstruction().Address
                _logger?EmulationError(errorAddress.ToString("X"), trace, e.Message)
            with _ -> ()

        // finally save the trace        
        stringExtracted