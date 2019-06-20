namespace ES.Tengu

open System
open System.Threading
open System.Collections.Generic
open ES.Sojobo
open B2R2

(*
- Create snapshot
- display memory hexview (accept register or address)
- edit memory (accept register or address)
- edit register
- hide/show disassembly
- hide/show IR
*)
type internal Command =
    | Step
    | Run
    | PrintRegisters
    | BreakpointList
    | BreakPoint of UInt64
    | DeleteBreakPoint of UInt64
    | NoCommand

type internal DebuggerState() =
    member val ProcessingCommands = false with get, set
    member val SteppingMode = false with get, set
    member val LastCommand = NoCommand with get, set

    member this.IsInInteractiveMode() =
        this.ProcessingCommands || this.SteppingMode

    member this.EnterDebuggerLoop() =
        this.SteppingMode <- false
        this.ProcessingCommands <- true
        
    member this.Run() =
        this.ProcessingCommands <- false
        this.SteppingMode <- false

    member this.Step() =
        this.ProcessingCommands <- false
        this.SteppingMode <- true

    member this.Break() =
        this.ProcessingCommands <- true

type Debugger(sandbox: ISandbox) =
    let _state = new DebuggerState()
    let _waitEvent = new ManualResetEventSlim()
    let _hooks = new Dictionary<UInt64, Hook>()

    let printRegisters() =
        let proc = sandbox.GetRunningProcess()
        Console.WriteLine()
        Console.WriteLine("-=[ Registers ]=-")
        ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"]
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

    let readCommand() =
        Console.Write("Command> ")
        let result = Console.ReadLine().Trim()
        if result.Equals("r", StringComparison.OrdinalIgnoreCase) then Run
        elif result.Equals("p", StringComparison.OrdinalIgnoreCase) then PrintRegisters
        elif result.Equals("s", StringComparison.OrdinalIgnoreCase) then Step
        elif result.Equals("bl", StringComparison.OrdinalIgnoreCase) then BreakpointList
        elif result.StartsWith("bp") then
            try BreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> NoCommand
        elif result.StartsWith("bc") then
            try DeleteBreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> NoCommand
        else NoCommand
                
    let parseCommand() =
        match _state.LastCommand with
        | PrintRegisters -> printRegisters()
        | BreakpointList -> listBreakpoints()
        | Run -> _state.Run()
        | Step -> _state.Step()
        | BreakPoint address -> 
            _hooks.[address] <- sandbox.AddHook(address, fun _ -> _state.Break())

        | DeleteBreakPoint address -> 
            match _hooks.TryGetValue(address) with
            | (true, hook) -> 
                _hooks.Remove(address) |> ignore
                sandbox.RemoveHook(hook)
            | _ -> ()

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