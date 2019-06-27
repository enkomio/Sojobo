namespace ES.Tengu

open System
open System.Threading
open System.Collections.Generic
open System.Text.RegularExpressions
open ES.Sojobo
open B2R2
open ES.Sojobo.Model
open B2R2.FrontEnd

(*
- Create snapshot
- set and read specific register with r
- k call stack
- disassemble (accept register or address)
*)
type internal Command =
    | Trace
    | Step
    | Go
    | PrintRegisters
    | BreakpointList
    | HideDisassembly
    | ShowDisassembly
    | ShowMemory of String
    | HideIr
    | ShowIr
    | BreakPoint of address:UInt64
    | DeleteBreakPoint of address:UInt64
    | SetRegister of name:String * value:UInt64
    | WriteMemory of address:UInt64 * size:Int32 * value:String
    | ShowHelp
    | NoCommand

type internal DebuggerState() =
    member val ProcessingCommands = false with get, set
    member val TracingMode = false with get, set
    member val StepAddress: UInt64 option = None with get, set
    member val LastCommand = NoCommand with get, set
    member val InstructionToEmulate: Instruction option = None with get, set

    member this.IsInInteractiveMode() =
        this.ProcessingCommands || this.TracingMode

    member this.EnterDebuggerLoop() =
        this.TracingMode <- false
        this.ProcessingCommands <- true
        this.StepAddress <- None
        
    member this.Go() =
        this.ProcessingCommands <- false
        this.TracingMode <- false

    member this.Trace() =
        this.ProcessingCommands <- false
        this.TracingMode <- true
        
    member this.Break() =
        this.ProcessingCommands <- true

type Debugger(sandbox: ISandbox) as this =
    let _state = new DebuggerState()
    let _waitEvent = new ManualResetEventSlim()
    let _hooks = new Dictionary<UInt64, Hook>()

    let printRegisters() =
        let proc = sandbox.GetRunningProcess()
        ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"; "ESP"; "EBP"; "EIP"]
        |> List.iter(fun register ->
            let address = proc.Cpu.GetRegister(register).Value |> BitVector.toUInt64
            let info =
                if proc.Memory.IsAddressMapped(address) && not(String.IsNullOrWhiteSpace(proc.Memory.GetMemoryRegion(address).Info))
                then String.Format("; {0} ", proc.Memory.GetMemoryRegion(address).Info)
                else String.Empty
            Console.WriteLine("{0}=0x{1} {2}", register, address.ToString("X"), info)
        )

    let listBreakpoints() =
        Console.WriteLine("-=[ Breakpoints ]=-")
        _hooks
        |> Seq.iter(fun kv -> Console.WriteLine("0x{0}", kv.Key.ToString("X")))

    let printHelp() =
        Console.WriteLine("Tengu debugger commands:")
        @"
            g                                   continue execution
            r                                   print register values
            t                                   execution trace
            p                                   execution step
            bl                                  list all breakpoints
            db <address>/<register> <size>      disaplay hex view
            hide <disassembly/ir>               hide the disassembly or IR during emulation
            show <disassembly/ir>               show the disassembly or IR during emulation
            bp <address>/<register>             set a breakpoint
            bc <address>                        clear a previously setted breakpoint
            set <register> <value>              set the value of a register
            eb <address> <value>                write memory, value in hex form, like: 01 02 03
            ew <address> <value>                write memory at address with word value
            ed <address> <value>                write memory at address with double word value
            eq <address> <value>                write memory at address with quad word value
            h/?                                 show this help
        ".Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map(fun line -> line.Trim())
        |> Array.iter(fun line -> Console.WriteLine("\t{0}", line))        

    let printHexView(startAddress: UInt64, buffer: Byte array) =
        buffer
        |> Array.chunkBySize 16
        |> Array.iteri(fun index chunk -> 
            let address = startAddress + uint64 (index * chunk.Length)
            let asciiString =
                chunk
                |> Array.map(fun b -> if b > 31uy && b < 127uy then char b else '.')
                |> fun chars -> new String(chars)
            Console.WriteLine("0x{0}  {1,-50}{2}", address, BitConverter.ToString(chunk).Replace('-', ' '), asciiString)
        )

    let parseTarget(target: String) =
        let proc = sandbox.GetRunningProcess()
        try Convert.ToUInt64(target, 16)            
        with _ -> proc.Cpu.GetRegister(target).Value |> BitVector.toUInt64

    let readCommand() =
        Console.Write("Command> ")
        let result = Console.ReadLine().Trim()
        if result.Equals("g", StringComparison.OrdinalIgnoreCase) then Go
        elif result.Equals("r", StringComparison.OrdinalIgnoreCase) then PrintRegisters
        elif result.Equals("t", StringComparison.OrdinalIgnoreCase) then Trace
        elif result.Equals("p", StringComparison.OrdinalIgnoreCase) then Step
        elif result.Equals("bl", StringComparison.OrdinalIgnoreCase) then BreakpointList
        elif result.Equals("help") || result.Equals("h", StringComparison.OrdinalIgnoreCase) || result.Equals("?", StringComparison.OrdinalIgnoreCase) then ShowHelp
        elif result.StartsWith("db") then ShowMemory result        
        elif result.StartsWith("hide") then
            let target = result.Split().[1].Trim()
            if target.Equals("disassembly", StringComparison.OrdinalIgnoreCase) then HideDisassembly
            elif target.Equals("ir", StringComparison.OrdinalIgnoreCase) then HideIr
            else NoCommand
        elif result.StartsWith("show") then
            let target = result.Split().[1].Trim()
            if target.Equals("disassembly", StringComparison.OrdinalIgnoreCase) then ShowDisassembly
            elif target.Equals("ir", StringComparison.OrdinalIgnoreCase) then ShowIr
            else NoCommand
        elif result.StartsWith("bp") then
            try BreakPoint (parseTarget(result.Split().[1]))
            with _ -> NoCommand
        elif result.StartsWith("bc") then
            try DeleteBreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> NoCommand        
        elif result.StartsWith("set") then 
            try
                let items = result.Split()
                SetRegister (items.[1].Trim(), Convert.ToUInt64(items.[2], 16))
            with _ -> NoCommand   
        elif result.StartsWith("e") && ['b'; 'w'; 'd'; 'q'] |> List.contains(result.[1]) then 
            try
                let items = result.Split()
                let size =
                    match result.[1] with
                    | 'b' -> 8
                    | 'w' -> 16
                    | 'd' -> 32
                    | 'q' -> 64
                    | _ -> 0
                WriteMemory (parseTarget(items.[1]), size, items.[2].Trim())
            with _ -> NoCommand  
        else NoCommand

    let addHook(address: UInt64) =
        _hooks.[address] <- sandbox.AddHook(address, fun _ -> _state.Break())

    let removeHook(address: UInt64) =
        match _hooks.TryGetValue(address) with
        | (true, hook) -> 
            _hooks.Remove(address) |> ignore
            sandbox.RemoveHook(hook)
        | _ -> ()

    let stepExecution() =
        let instruction = sandbox.GetRunningProcess().GetInstruction()
        if instruction.IsCall() then
            let nextInstructionAddress = 
                instruction.Address + 
                uint64 instruction.Length
            addHook(nextInstructionAddress)
            _state.StepAddress <- Some nextInstructionAddress
            _state.Go()
        else
            _state.Trace()
                
    let parseCommand() =
        match _state.LastCommand with
        | PrintRegisters -> printRegisters()
        | BreakpointList -> listBreakpoints()
        | ShowHelp -> printHelp()
        | Go -> _state.Go()
        | Trace -> _state.Trace()
        | Step -> stepExecution()
        | HideDisassembly -> this.PrintDisassembly <- false
        | HideIr -> this.PrintIR <- false
        | ShowDisassembly -> this.PrintDisassembly <- true
        | ShowIr -> this.PrintIR <- true
        | BreakPoint address -> addHook(address)
        | DeleteBreakPoint address -> removeHook(address)            
        | ShowMemory command ->
            let items = command.Split()
            if items.Length >= 2 then
                let address = parseTarget(items.[1])
                let length = if items.Length < 3 then (8 * 5) else Int32.Parse(items.[2])
                let buffer = sandbox.GetRunningProcess().Memory.ReadMemory(address, length)
                printHexView(address, buffer)
        | SetRegister (registerName, value) ->
            try
                let proc = sandbox.GetRunningProcess()
                let register = proc.Cpu.GetRegister(registerName)
                let bvValue =
                    match register.Type with
                    | EmulatedType.Byte -> Model.createByte(byte value)
                    | EmulatedType.Word -> Model.createUInt16(uint16 value)
                    | EmulatedType.DoubleWord -> Model.createUInt32(uint32 value)
                    | EmulatedType.QuadWord -> Model.createUInt64(value)
                    | _ -> failwith "invalid Size"
                proc.Cpu.SetRegister({register with Value = bvValue.Value})
            with _ -> ()            
        | WriteMemory (address, size, rawValue) ->
            let mem = sandbox.GetRunningProcess().Memory
            if size = 8 && Regex.IsMatch(rawValue, "[a-fA-F0-9][a-fA-F0-9](\\b[a-fA-F0-9][a-fA-F0-9])*") then
                // if it in the format: 01 02 03 04 05 06
                rawValue.Split() |> Array.map(fun hex -> Convert.ToByte(hex, 16))                
            elif size = 16 then
                BitConverter.GetBytes(Convert.ToUInt16(rawValue, 16))
            elif size = 32 then
                BitConverter.GetBytes(Convert.ToUInt32(rawValue, 16))
            elif size = 64 then
                BitConverter.GetBytes(Convert.ToUInt64(rawValue, 16))
            else Array.empty<Byte>
            |> fun value -> mem.WriteMemory(address, value)
        | _ -> _state.LastCommand <- NoCommand

    let readBreakCommand() =  
        while true do
            if _state.IsInInteractiveMode() then
                // wait for debug loop to finish
                _waitEvent.Wait()
                _waitEvent.Reset()
            elif Console.KeyAvailable then
                if Console.ReadKey(true).KeyChar = 'b' 
                then _state.Break()
            else
                Thread.Sleep(100)

    let debuggerLoop() =
        while _state.ProcessingCommands do
            match readCommand() with
            | NoCommand -> ()
            | c -> _state.LastCommand <- c
            
            parseCommand()
        _waitEvent.Set()

    let writeDisassembly(proc: IProcessContainer) =
        let text = ES.Sojobo.Utility.formatCurrentInstruction(proc)
        Console.WriteLine(text)

    let writeIR(proc: IProcessContainer) =
        ES.Sojobo.Utility.formatCurrentInstructionIR(proc)
        |> Array.iter(Console.WriteLine)

    let removeStepHook() =
        match _state.StepAddress with
        | Some address -> removeHook(address)
        | _ -> ()

    member val PrintDisassembly = false with get, set
    member val PrintIR = false with get, set

    member this.BeforeEmulation() =
        let proc = sandbox.GetRunningProcess()        
        if this.PrintDisassembly then writeDisassembly(proc)
        if this.PrintIR then writeIR(proc)

        _state.InstructionToEmulate <- proc.GetInstruction() |> Some

        // check if must enter debugger loop
        if _state.IsInInteractiveMode() then
            removeStepHook()
            _state.EnterDebuggerLoop()            
            debuggerLoop()

    member this.AfterEmulation() =
        ()

    member this.Start() = 
        ignore (async { readBreakCommand() } |> Async.StartAsTask)
