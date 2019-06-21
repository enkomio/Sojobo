namespace ES.Tengu

open System
open System.Threading
open System.Collections.Generic
open ES.Sojobo
open B2R2
open ES.Sojobo
open ES.Sojobo
open ES.Sojobo

(*
- Create snapshot
- edit memory (accept register or address)
- disassemble (accept register or address)
*)
type internal Command =
    | Trace
    | Go
    | PrintRegisters
    | BreakpointList
    | HideDisassembly
    | ShowDisassembly
    | ShowMemory of String
    | HideIr
    | ShowIr
    | BreakPoint of UInt64
    | DeleteBreakPoint of UInt64
    | SetRegister of String * UInt64
    | NoCommand

type internal DebuggerState() =
    member val ProcessingCommands = false with get, set
    member val TracingMode = false with get, set
    member val LastCommand = NoCommand with get, set

    member this.IsInInteractiveMode() =
        this.ProcessingCommands || this.TracingMode

    member this.EnterDebuggerLoop() =
        this.TracingMode <- false
        this.ProcessingCommands <- true
        
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
        elif result.Equals("bl", StringComparison.OrdinalIgnoreCase) then BreakpointList
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
            try BreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> NoCommand
        elif result.StartsWith("bc") then
            try DeleteBreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> NoCommand        
        elif result.StartsWith("set") then 
            try
                let items = result.Split()
                SetRegister (items.[1].Trim(), Convert.ToUInt64(items.[2], 16))
            with _ -> NoCommand        
        else NoCommand
                
    let parseCommand() =
        match _state.LastCommand with
        | PrintRegisters -> printRegisters()
        | BreakpointList -> listBreakpoints()
        | Go -> _state.Go()
        | Trace -> _state.Trace()
        | HideDisassembly -> this.PrintDisassembly <- false
        | HideIr -> this.PrintIR <- false
        | ShowDisassembly -> this.PrintDisassembly <- true
        | ShowIr -> this.PrintIR <- true
        | BreakPoint address -> 
            _hooks.[address] <- sandbox.AddHook(address, fun _ -> _state.Break())
        | DeleteBreakPoint address -> 
            match _hooks.TryGetValue(address) with
            | (true, hook) -> 
                _hooks.Remove(address) |> ignore
                sandbox.RemoveHook(hook)
            | _ -> ()
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
                    if proc.GetPointerSize() = 32 then Model.createUInt32(uint32 value)
                    else Model.createUInt64(value)                
                proc.Cpu.SetRegister({register with Value = bvValue.Value})
            with _ -> ()            
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
        printRegisters()
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

    member val PrintDisassembly = false with get, set
    member val PrintIR = false with get, set

    member this.Process() =
        if _state.IsInInteractiveMode() then
            _state.EnterDebuggerLoop()
            debuggerLoop()

        let proc = sandbox.GetRunningProcess()
        if this.PrintDisassembly then writeDisassembly(proc)
        if this.PrintIR then writeIR(proc)

    member this.Start() = 
        ignore (async { readBreakCommand() } |> Async.StartAsTask)