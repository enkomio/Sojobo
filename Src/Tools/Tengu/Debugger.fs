namespace ES.Tengu

open System
open System.Threading
open ES.Sojobo
open B2R2

type internal Command =
    | Step
    | Run
    | PrintRegisters
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

    let printRegisters() =
        let proc = sandbox.GetRunningProcess()
        Console.WriteLine()
        Console.WriteLine("-=[ Registers ]=-")
        ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"]
        |> List.iter(fun register ->
            let address = proc.Cpu.GetRegister(register).Value |> BitVector.toUInt64
            let region =
                if proc.Memory.IsAddressMapped(address)
                then proc.Memory.GetMemoryRegion(address).BaseAddress
                else 0UL
            Console.WriteLine("{0}=[{1}]:{2}", register, region, address)            
        )

    let readCommand() =
        Console.Write("Command> ")
        let result = Console.ReadLine()
        if result.StartsWith("r") then Run
        elif result.StartsWith("p") then PrintRegisters
        elif result.StartsWith("s") then Step
        else NoCommand
                
    let parseCommand() =
        match _state.LastCommand with
        | PrintRegisters -> printRegisters()
        | Run -> _state.Run()
        | Step -> _state.Step()
        | _ -> ()

    let readBreakCommand() =        
        if _state.IsInInteractiveMode() then
            // wait for debug loop to finish
            _waitEvent.Wait()
            _waitEvent.Reset()
        else
            if Console.ReadKey(true).KeyChar = 'b' 
            then _state.Break()

    let debuggerLoop() =    
        printRegisters()
        while _state.ProcessingCommands do
            match readCommand() with
            | NoCommand -> ()
            | c -> _state.LastCommand <- c
            
            parseCommand()
        _waitEvent.Set()

    member this.Process() =
        if _state.IsInInteractiveMode() then
            _state.EnterDebuggerLoop()
            debuggerLoop()

    member this.Start() = 
        ignore (async { while true do readBreakCommand() } |> Async.StartAsTask)