namespace ES.Tengu

open System
open System.IO
open System.Threading
open System.Collections.Generic
open ES.Sojobo
open B2R2
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2.FrontEnd.Intel
open System.Text.RegularExpressions
open Newtonsoft.Json

(*
- display running information, like the numberd of executed instruction, execution time and mean time time to execute 1 instruction (do performance test with and without cache)
*)
type internal Command =
    | Trace
    | Step
    | Go
    | CallStack of count:Int32
    | PrintRegisters
    | BreakpointList
    | HideDisassembly
    | ShowDisassembly
    | ShowMemory of address:UInt64 * size:Int32 * length:Int32 option
    | MemoryMap
    | HideIr
    | ShowIr
    | Disassemble of address:UInt64 * size:Int32
    | BreakPoint of address:UInt64
    | DeleteBreakPoint of address:UInt64
    | SetRegister of name:String * value:UInt64
    | ShowRegister of name:String
    | WriteMemory of address:UInt64 * size:Int32 * value:String
    | DumpMemory of address:UInt64 * size:Int32 * filename:String
    | Comment of address:UInt64 * comment:String
    | SaveSnapshot of name:String
    | LoadSnapshot of name:String
    | ShowHelp
    | NoCommand
    | Error

type DebuggerSnapshot = {
    Breakpoints: UInt64 array
    Comments: String array
}

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
    let _comments = new Dictionary<UInt64, String>()

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

    let printCallStack(count: Int32) =
        Console.WriteLine("-=[ Call stack ]=-")
        sandbox.GetRunningProcess().GetCallStack()
        |> Array.truncate count
        |> Array.iteri(fun index address ->
            Console.WriteLine("{0}: 0x{1}", index + 1, address.ToString("X"))
        )

    let dumpMemory(address: UInt64, size: Int32, filename: String) =
        let content = sandbox.GetRunningProcess().Memory.ReadMemory(address, size)
        File.WriteAllBytes(filename, content)
        
    let showMemoryMap() =
        Console.WriteLine("-=[ Memory Map ]=-")
        let header = 
            String.Format(
                "{0,-12} | {1,-12} | {2,-10} | {3,-25} | {4,-35} | {5}",
                "Base Address", 
                "End Address",
                "Size",
                "Permission",
                "Info",
                "Type"                
            )

        let contentLines =
            sandbox.GetRunningProcess().Memory.GetMemoryMap()
            |> Array.map(fun region ->
                String.Format(
                    "0x{0,-10} | 0x{1,-10} | {2,-10} | {3,-25} | {4,-35} | {5}",
                    region.BaseAddress.ToString("X"), 
                    (region.BaseAddress + uint64 region.Content.Length).ToString("X"),
                    region.Content.Length,
                    region.Permission,
                    region.Info,
                    region.Type                
                )
            )

        let length = min Console.WindowWidth (contentLines |> Array.maxBy(fun s -> s.Length)).Length

        // print
        Console.WriteLine(header)
        Console.WriteLine(String.Empty.PadRight(length, '-'))
        contentLines |> Array.iter(Console.WriteLine)

    let printDisassembly(address: UInt64, count: Int32) =
        let proc = sandbox.GetRunningProcess()
        let mutable offset = address
        for i=0 to count-1 do
            let instruction = proc.GetInstruction(offset)
            offset <- offset + uint64 instruction.Length
            match _comments.TryGetValue(instruction.Address) with
            | (true, text) -> String.Format("{0} ; {1}", ES.Sojobo.Utility.disassemble(proc, instruction), text)
            | _ -> ES.Sojobo.Utility.disassemble(proc, instruction)
            |> Console.WriteLine

    let printHelp() =
        Console.WriteLine("Tengu debugger commands:")
        @"
            g                                   continue execution
            r                                   print register values
            t                                   execution trace
            p                                   execution step
            bl                                  list all breakpoints
            k [<frame count>]                   call stack
            db <address/register> <size>        disaplay hex view
            dw <address>                        display word at address
            dd <address>                        display double word at address
            dq <address>                        display quad word at address
            hide <disassembly/ir>               hide the disassembly or IR during emulation
            show <disassembly/ir>               show the disassembly or IR during emulation
            comment <address> <value>           add a comment to the specified address
            bp <address/register>               set a breakpoint
            bc <address>                        clear a previously setted breakpoint
            u [<address/register>] [count]      disassemble the bytes at the specified address (if specified otherwise at PC)
            r <register> [<value>]              show the value of a register or set its value
            eb <address> <value>                write memory, value in hex form, like: 01 02 03
            ew <address> <value>                write memory at address with word value
            ed <address> <value>                write memory at address with double word value
            eq <address> <value>                write memory at address with quad word value    
            save <filename>                     save a snapshot to the given filename
            load <filename>                     load a snapshot from the given filename
            address                             show memory map
            dump <filename> <addr> <size>       save memory to file
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
        let result = Console.ReadLine().Trim().ToLowerInvariant()
        if result.Equals("g", StringComparison.OrdinalIgnoreCase) then Go
        elif result.Equals("r", StringComparison.OrdinalIgnoreCase) then PrintRegisters
        elif result.Equals("t", StringComparison.OrdinalIgnoreCase) then Trace
        elif result.Equals("p", StringComparison.OrdinalIgnoreCase) then Step
        elif result.Equals("bl", StringComparison.OrdinalIgnoreCase) then BreakpointList
        elif result.Equals("address", StringComparison.OrdinalIgnoreCase) then MemoryMap
        elif result.Equals("help") || result.Equals("h", StringComparison.OrdinalIgnoreCase) || result.Equals("?", StringComparison.OrdinalIgnoreCase) then ShowHelp                
        elif result.StartsWith("hide") then
            let target = result.Split().[1].Trim()
            if target.Equals("disassembly", StringComparison.OrdinalIgnoreCase) then HideDisassembly
            elif target.Equals("ir", StringComparison.OrdinalIgnoreCase) then HideIr
            else Error
        elif result.StartsWith("show") then
            let target = result.Split().[1].Trim()
            if target.Equals("disassembly", StringComparison.OrdinalIgnoreCase) then ShowDisassembly
            elif target.Equals("ir", StringComparison.OrdinalIgnoreCase) then ShowIr
            else Error
        elif result.StartsWith("comment") then
            let items = result.Split()
            if items.Length >= 3 
            then Comment(parseTarget(items.[1]), String.Join(" ", items.[2..]))
            elif items.Length = 2 then Comment(parseTarget(items.[1]), String.Empty)
            else Error
        elif result.StartsWith("bp") then
            try BreakPoint (parseTarget(result.Split().[1]))
            with _ -> Error
        elif result.StartsWith("bc") then
            try DeleteBreakPoint (Convert.ToUInt64(result.Split().[1], 16))
            with _ -> Error        
        elif result.StartsWith("save") then
            try SaveSnapshot(result.Split().[1])
            with _ -> Error   
        elif result.StartsWith("load") then
            try LoadSnapshot(result.Split().[1])
            with _ -> Error   
        elif result.StartsWith("r") && result.Length > 1 then 
            try
                let items = result.Split()
                if items.Length = 2
                then ShowRegister(items.[1].Trim())
                else SetRegister (items.[1].Trim(), Convert.ToUInt64(items.[2], 16))
            with _ -> Error   
        elif result.StartsWith("u") then
            try 
                let items = result.Split()
                let address = 
                    if items.Length > 1 
                    then parseTarget(items.[1]) 
                    else sandbox.GetRunningProcess().ProgramCounter.Value |> BitVector.toUInt64
                let count = if items.Length > 2 then Int32.Parse(items.[2]) else 10
                Disassemble (address, count)
            with _ -> Error
        elif result.StartsWith("k") || result.Equals("k", StringComparison.OrdinalIgnoreCase) then
            try 
                let count = if result.Length = 1 then 10 else Int32.Parse(result.Split().[1])
                CallStack(count)
            with _ -> Error
        elif result.StartsWith("d") && ['b'; 'w'; 'd'; 'q'] |> List.contains(result.[1]) then
            try
                let items = result.Split()
                let size =
                    match result.[1] with
                    | 'b' -> 8
                    | 'w' -> 16
                    | 'd' -> 32
                    | 'q' -> 64
                    | _ -> 0

                let length =
                    if size = 8 
                    then Some (if items.Length < 3 then (8 * 5) else Int32.Parse(items.[2]))
                    else None

                ShowMemory (parseTarget(items.[1]), size, length)
            with _ -> Error  
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
                WriteMemory (parseTarget(items.[1]), size, String.Join(" ", items.[2..]).Trim())
            with _ -> Error  
        elif result.StartsWith("dump") then 
            try
                let items = result.Split()
                DumpMemory (parseTarget(items.[1]), Int32.Parse(items.[2]), items.[3])
            with _ -> Error  
        elif String.IsNullOrWhiteSpace(result) then NoCommand
        else Error

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

    let addComment(address: UInt64, text: String) =
        if String.IsNullOrWhiteSpace(text) then
            if _comments.ContainsKey(address) 
            then _comments.Remove(address) |> ignore
        else _comments.[address] <- text

    let saveSnapshot(filename: String) =
        let snapshotManager = new SnapshotManager(sandbox :?> BaseSandbox)
        let snapshot = snapshotManager.TakeSnaphot()
        snapshot.SaveTo(filename)

        // save comments
        let debuggerSnapshot = {
            Comments = 
                _comments 
                |> Seq.map(fun kv -> String.Format("{0}|{1}", kv.Key, kv.Value)) 
                |> Seq.toArray

            Breakpoints =
                sandbox.GetHooks()
                |> Array.map(sandbox.GetHookAddress)
                |> Array.choose(id)
        }

        let serializedDebuggerSnapshot = JsonConvert.SerializeObject(debuggerSnapshot, Formatting.Indented)
        File.WriteAllText(filename + ".json", serializedDebuggerSnapshot)

    let loadSnapshot(filename: String) =
        try
            // load snapshot
            let snapshotManager = new SnapshotManager(sandbox :?> BaseSandbox)
            snapshotManager.LoadSnapshot(Snapshot.Read(filename))

            // load debugger state
            let debuggerStateJson = File.ReadAllText(filename + ".json")
            let debuggerState = JsonConvert.DeserializeObject<DebuggerSnapshot>(debuggerStateJson)

            // set breakpoints
            debuggerState.Breakpoints
            |> Array.iter(addHook)

            // set comments
            debuggerState.Comments
            |> Array.map(fun s -> s.Split('|'))
            |> Array.iter(fun items -> 
                let address = UInt64.Parse(items.[0])
                let text = String.Join("|", items.[1..])
                addComment(address, text)
            )
        with _ -> ()    

    let parseCommand() =
        match _state.LastCommand with
        | PrintRegisters -> printRegisters()
        | BreakpointList -> listBreakpoints()
        | ShowHelp -> printHelp()
        | Go -> _state.Go()
        | Trace -> _state.Trace()
        | Step -> stepExecution()
        | MemoryMap -> showMemoryMap()
        | HideDisassembly -> this.PrintDisassembly <- false
        | HideIr -> this.PrintIR <- false
        | ShowDisassembly -> this.PrintDisassembly <- true
        | ShowIr -> this.PrintIR <- true
        | SaveSnapshot filename -> saveSnapshot(filename)
        | LoadSnapshot filename -> loadSnapshot(filename)
        | BreakPoint address -> addHook(address)
        | DeleteBreakPoint address -> removeHook(address)
        | DumpMemory (addr, size, filename) -> dumpMemory(addr, size, filename)
        | CallStack count -> printCallStack(count)
        | Disassemble(address, count) -> printDisassembly(address, count)
        | Comment(address, text) -> addComment(address, text)
        | ShowMemory (address, size, length) ->
            let mem = sandbox.GetRunningProcess().Memory
            if size = 8 then
                let buffer = mem.ReadMemory(address, length.Value)                
                printHexView(address, buffer)
            elif size = 16 then
                let num = mem.ReadMemory<UInt16>(address)
                Console.WriteLine("0x{0}  0x{1}", address, num.ToString("X"))
            elif size = 32 then
                let num = mem.ReadMemory<UInt32>(address)
                Console.WriteLine("0x{0}  0x{1}", address, num.ToString("X"))
            elif size = 64 then
                let num = mem.ReadMemory<UInt64>(address)
                Console.WriteLine("0x{0}  0x{1}", address, num.ToString("X"))
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
        | ShowRegister(regName) ->
            try
                let proc = sandbox.GetRunningProcess()
                let register = Enum.Parse(typeof<Register>, regName.ToUpperInvariant()) :?> Register 
                let extendedRegister = Register.extendRegister32 register

                let rawValue = 
                    proc.Cpu.GetRegister(extendedRegister.ToString()).Value 
                    |> BitVector.toUInt64
                
                let regValue =
                    match register |> Register.toRegType with
                    | 8<rt> -> (byte rawValue).ToString("X")
                    | 16<rt> -> (uint16 rawValue).ToString("X")
                    | 32<rt> -> (uint32 rawValue).ToString("X")
                    | 64<rt> -> (rawValue).ToString("X")
                    | _ -> failwith "invalid Size"

                Console.WriteLine("{0} = 0x{1}", regName, regValue)
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
        | _ -> ()

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
            | NoCommand -> () // repeat the previous command            
            | c -> _state.LastCommand <- c
            
            parseCommand()
        _waitEvent.Set()

    let writeDisassembly(proc: IProcessContainer) =
        let instruction = ES.Sojobo.Utility.disassemble(proc, proc.GetInstruction())
        let pc = proc.ProgramCounter.Value |> BitVector.toUInt64
        match _comments.TryGetValue(pc) with
        | (true, text) -> String.Format("{0} ; {1}", instruction, text)
        | _ -> instruction
        |> Console.WriteLine

    let writeIR(proc: IProcessContainer) =
        ES.Sojobo.Utility.disassembleCurrentInstructionIR(proc)
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

    member this.Break() =
        _state.Break()
            
    member this.Start() = 
        ignore (async { readBreakCommand() } |> Async.StartAsTask)
