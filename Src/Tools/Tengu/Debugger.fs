namespace ES.Tengu

open System
open System.Reflection
open System.IO
open System.Threading
open System.Collections.Generic
open System.Collections.Concurrent
open ES.Sojobo
open ES.Sojobo.MemoryUtility
open B2R2
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2.FrontEnd.Intel
open System.Text.RegularExpressions
open Newtonsoft.Json
open System.Text

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
    | DisplayType of address:UInt64 * typeName:String option
    | MemoryMap
    | HideIr
    | ShowIr
    | Disassemble of address:UInt64 * size:Int32
    | BreakPoint of address:UInt64 * command:String option
    | DeleteBreakPoint of address:UInt64
    | SetRegister of name:String * value:UInt64
    | ShowRegister of name:String
    | WriteMemory of address:UInt64 * size:Int32 * value:String
    | DumpMemory of address:UInt64 * size:Int32 * filename:String
    | Comment of address:UInt64 * comment:String
    | SaveSnapshot of name:String
    | LoadSnapshot of name:String
    | Echo of message:String
    | ShowAsciiString of address:UInt64
    | Script of filename:String
    | ShowHelp
    | NoCommand
    | Error

type DebuggerSnapshot = {
    Breakpoints: (UInt64 * String) array
    Comments: String array
}

type internal DebuggerState() =
    member val ProcessingCommands = false with get, set
    member val TracingMode = false with get, set
    member val StepAddress: UInt64 option = None with get, set
    member val LastCommand = NoCommand with get, set
    member val InstructionToEmulate: Instruction option = None with get, set
    member val ForcePrint = false with get, set

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

type BreakPoint = {
    Address: UInt64
    Command: String option
    Hook: Hook
}

type Debugger(sandbox: ISandbox) as this =
    let _state = new DebuggerState()
    let _waitEvent = new ManualResetEventSlim()
    let _breakpoints = new Dictionary<UInt64, BreakPoint>()
    let _comments = new Dictionary<UInt64, String>()
    let _commandQueue = new ConcurrentQueue<String>()
    let mutable _lastCommandString = String.Empty

    let printRegisters() =
        let proc = sandbox.GetRunningProcess()
        if proc.PointerSize = 32 then ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"; "ESP"; "EBP"; "EIP"]
        else ["RAX"; "RBX"; "RCX"; "RDX"; "RSI"; "RDI"; "RSP"; "RBP"; "RIP"; "R8"; "R9"; "R10"; "R11"; "R12"; "R13"; "R14"; "R15"]
        |> List.iter(fun register ->
            let address = proc.Cpu.GetRegister(register).Value |> BitVector.toUInt64
            let info =
                match proc.Memory.GetMemoryRegion(address) with
                | Some region when not(String.IsNullOrWhiteSpace(region.Info)) -> String.Format("; {0} ", region.Info)
                | _ -> String.Empty
            Console.WriteLine("{0}=0x{1} {2}", register, address.ToString("X"), info)
        )

    let listBreakpoints() =
        Console.WriteLine("-=[ Breakpoints ]=-")
        _breakpoints
        |> Seq.iter(fun kv -> 
            Console.WriteLine("0x{0} {1}", kv.Key.ToString("X"), defaultArg kv.Value.Command String.Empty)
        )

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
        try
            let proc = sandbox.GetRunningProcess()
            let mutable offset = address
            for i=0 to count-1 do
                let instruction = proc.GetInstruction(offset)
                offset <- offset + uint64 instruction.Length
                match _comments.TryGetValue(instruction.Address) with
                | (true, text) -> String.Format("{0} ; {1}", ES.Sojobo.Utility32.disassemble(proc, instruction), text)
                | _ -> ES.Sojobo.Utility32.disassemble(proc, instruction)
                |> Console.WriteLine
        with e ->
            Console.Error.WriteLine(e)

    let displayType(address: UInt64, typeName: String option) =
        let proc = sandbox.GetRunningProcess()
        match typeName with
        | Some typeName -> 
            AppDomain.CurrentDomain.GetAssemblies()
            |> Seq.collect(fun assembly -> assembly.GetTypes())
            |> Seq.filter(fun t -> t.Name.Equals(typeName))
            |> Seq.tryHead
            |> function 
                | Some objectType ->    
                    try
                        Console.WriteLine("-=[Object type: {0} ]=-", objectType)
                        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public 
                        let obj = proc.Memory.ReadMemory(address, objectType)
                        let mutable offset = 0
                        obj.GetType().GetFields(flags)
                        |> Array.iter(fun field ->        
                            let fieldValue = field.GetValue(obj)
                            let stringValue =
                                if field.FieldType.IsArray then "Array"
                                elif field.FieldType.IsPrimitive then fieldValue.ToString()
                                elif field.FieldType.IsClass then "Ptr " + field.FieldType.Name
                                else "(N/A)"
                            Console.WriteLine("\t+0x{0} {1} : {2}", offset.ToString("X"), field.Name, stringValue)
                            offset <- 
                                (if field.FieldType.IsArray then Helpers.getFieldArrayLength(field, proc.PointerSize, new Dictionary<Type, Int32>())
                                else Helpers.deepSizeOf(fieldValue.GetType(), proc.PointerSize)) 
                                + offset
                        )
                    with e -> 
                        Console.Error.WriteLine(e)
                | None -> ()
        | _ ->
            proc.TryGetSymbol(address)
            |> Option.iter(fun symbol ->
                Console.WriteLine("0x{0}: {1} [{2}]", address.ToString("X"), symbol.Name, symbol.LibraryName)
            )

    let printHelp() =
        Console.WriteLine("Tengu debugger commands:")
        @"
            g                                   continue execution
            r                                   print register values
            r <register> [<value>]              show the value of a register or set its value
            t                                   execution trace
            p                                   execution step
            bl                                  list all breakpoints
            k [<frame count>]                   call stack
            db <address/register> <size>        disaplay hex view
            dw <address/register>               display word at address
            dd <address/register>               display double word at address
            dq <address/register>               display quad word at address
            da <address/register>               display the ASCII string at the given address
            dt <address/register> [type]        display information about a local variable, global variable or data type
            hide <disassembly/ir>               hide the disassembly or IR during emulation
            show <disassembly/ir>               show the disassembly or IR during emulation
            comment <address> <value>           add a comment to the specified address
            bp <address/register> [cmd]         set a breakpoint, it is possible to specify an optional command to execute
            bc <address>                        clear a previously setted breakpoint
            u [<address/register>] [count]      disassemble the bytes at the specified address (if specified otherwise at PC)            
            eb <address> <value>                write memory, value in hex form, like: 01 02 03
            ew <address> <value>                write memory at address with word value
            ed <address> <value>                write memory at address with double word value
            eq <address> <value>                write memory at address with quad word value    
            save <filename>                     save a snapshot to the given filename
            load <filename>                     load a snapshot from the given filename
            script <filename>                   load commands from the given filename
            address                             show memory map
            dump <filename> <addr> <size>       save memory to file
            .echo <message>                     print the message to the debugger console
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
            Console.WriteLine("0x{0}  {1,-50}{2}", address.ToString("X"), BitConverter.ToString(chunk).Replace('-', ' '), asciiString)
        )

    let parseTarget(target: String) =
        let proc = sandbox.GetRunningProcess()
        try Convert.ToUInt64(target, 16)            
        with _ -> proc.Cpu.GetRegister(target).Value |> BitVector.toUInt64

    let parseSingleCommandString(rawResult: String) =
        let result = rawResult.Trim()        
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
        elif result.StartsWith(".echo") then
            Echo(String.Join(" ", result.Split().[1..]))
        elif result.StartsWith("bp") then
            try 
                let items = result.Split()
                let addr = parseTarget(items.[1])                
                let command = if items.Length > 2 then Some (String.Join(" ", items.[2..])) else None
                BreakPoint (addr, command)
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
        elif result.StartsWith("script") then
            try Script(result.Split().[1])
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
        elif result.StartsWith("k") then
            try 
                let count = if result.Length = 1 then 10 else Int32.Parse(result.Split().[1])
                CallStack(count)
            with _ -> Error   
        elif result.StartsWith("dt") then
            try
                let items = result.Split()
                let address = parseTarget(items.[1])
                let typeName = if items.Length > 2 then Some(items.[2]) else None
                DisplayType(address, typeName)
            with _ -> Error  
        elif result.StartsWith("d") && ['a'; 'b'; 'w'; 'd'; 'q'] |> List.contains(result.[1]) then
            try
                let modifier = result.[1]
                let items = result.Split()
                if modifier = 'a' then
                    ShowAsciiString(parseTarget(items.[1]))
                else
                    let size =
                        match modifier with
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

    let parseCommandString(str: String) =
        let currentCommand = new StringBuilder()
        let mutable escape = false
        
        str
        |> Seq.iter(fun c -> 
            if escape then 
                escape <- false
                currentCommand.Append(c) |> ignore
            elif c = '\\' then
                escape <- true
            elif c = ';' then
                let command = currentCommand.ToString().Trim()
                currentCommand.Clear() |> ignore
                if not(String.IsNullOrWhiteSpace(command)) then
                    _commandQueue.Enqueue(command)
            else
                currentCommand.Append(c) |> ignore
        )

        // add last command
        let command = currentCommand.ToString().Trim()
        if not(String.IsNullOrWhiteSpace(command)) then
            _commandQueue.Enqueue(command)

    let rec readCommands() =
        if _commandQueue.Count > 0 then
            let command = ref(String.Empty)
            _commandQueue.TryDequeue(command) |> ignore
            !command
        else
            let d = DateTime.Now.ToString("hh:mm:ss")
            Console.Write("[{0}]> ", d)
            let commandString = Console.ReadLine().Trim()
            parseCommandString(commandString)            
            if _commandQueue.Count > 0 then
                _lastCommandString <- commandString
                let command = ref(String.Empty)
                _commandQueue.TryDequeue(command) |> ignore
                !command
            else
                String.Empty

    let addBreakpoint(address: UInt64, command: String option) =
        _breakpoints.[address] <- {
            Address = address
            Command = command
            Hook = 
                sandbox.AddHook(
                    address, 
                    fun _ -> 
                        _state.ForcePrint <- true
                        _state.Break()
                )
        }

    let removeBreakpoint(address: UInt64) =
        // force remove by address
        sandbox.GetHooks()
        |> Seq.iter(fun hook ->
            match hook with
            | Address (addr, _) when addr = address -> sandbox.RemoveHook(hook)
            | _ -> ()
        )

        match _breakpoints.TryGetValue(address) with
        | (true, breakpoint) -> 
            _breakpoints.Remove(address) |> ignore
            sandbox.RemoveHook(breakpoint.Hook)            
        | _ -> ()

    let stepExecution() =
        let instruction = sandbox.GetRunningProcess().GetInstruction()
        if instruction.IsCall() then
            let nextInstructionAddress = instruction.Address + uint64 instruction.Length
            addBreakpoint(nextInstructionAddress, None)
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
        Console.WriteLine("-=[ Saving Process Snapshot ]=-")
        snapshot.VirtualAddressSpace
        |> Array.iter(fun memRegion ->
            Console.WriteLine("0x{0} - 0x{1} Size: 0x{2}", 
                memRegion.BaseAddress.ToString("X"), 
                (memRegion.BaseAddress + uint64 memRegion.Content.Length).ToString("X"),
                memRegion.Content.Length.ToString("X")
            )
        )

        snapshot.SaveTo(filename)

        // save comments
        let debuggerSnapshot = {
            Comments = 
                _comments 
                |> Seq.map(fun kv -> String.Format("{0}|{1}", kv.Key, kv.Value)) 
                |> Seq.toArray

            Breakpoints = 
                _breakpoints 
                |> Seq.map(fun bp -> (bp.Key, defaultArg bp.Value.Command String.Empty))
                |> Seq.toArray
        }

        let serializedDebuggerSnapshot = JsonConvert.SerializeObject(debuggerSnapshot, Formatting.Indented)
        File.WriteAllText(filename + ".json", serializedDebuggerSnapshot)

    let loadScript(filename: String) =
        try
            File.ReadAllLines(filename)
            |> Array.map(fun l -> l.Trim())
            |> Array.filter(fun l  -> (l.StartsWith("#") || l.StartsWith("//") || String.IsNullOrWhiteSpace(l)) |> not)
            |> Array.iter(fun command -> 
                Console.WriteLine("Cmd: {0}", command)
                parseCommandString(command)
            )
        with e ->
            Console.Error.WriteLine("Exception: {0}", e) 

    let loadSnapshot(filename: String) =
        try
            // load snapshot
            let snapshotManager = new SnapshotManager(sandbox :?> BaseSandbox)
            snapshotManager.LoadSnapshot(Snapshot.Read(filename))

            // load debugger state
            let debuggerStateJson = File.ReadAllText(filename + ".json")
            let debuggerState = JsonConvert.DeserializeObject<DebuggerSnapshot>(debuggerStateJson)

            // set breakpoints
            _breakpoints.Clear()
            debuggerState.Breakpoints
            |> Array.iter(fun (addr, cmd) ->
                addBreakpoint(addr, if String.IsNullOrWhiteSpace(cmd) then None else Some cmd)
            )

            // set comments
            _comments.Clear()
            debuggerState.Comments
            |> Array.map(fun s -> s.Split('|'))
            |> Array.iter(fun items -> 
                let address = UInt64.Parse(items.[0])
                let text = String.Join("|", items.[1..])
                addComment(address, text)
            )
        with e ->
            Console.Error.WriteLine("Exception: {0}", e)            
        
    let showAsciiString(addr: UInt64) =
        readAsciiString(sandbox.GetRunningProcess().Memory, addr)
        |> Console.WriteLine

    let showMemory(address: UInt64, size: Int32, length: Int32 option) =
        try
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
        | Script filename -> loadScript(filename)
        | BreakPoint (address, cmd) -> addBreakpoint(address, cmd)
        | DeleteBreakPoint address -> removeBreakpoint(address)
        | DumpMemory (addr, size, filename) -> dumpMemory(addr, size, filename)
        | CallStack count -> printCallStack(count)
        | ShowAsciiString addr -> showAsciiString(addr)
        | Disassemble(address, count) -> printDisassembly(address, count)
        | Comment(address, text) -> addComment(address, text)
        | Echo message -> Console.WriteLine(message)
        | DisplayType(address, typeName) -> displayType(address, typeName)
        | ShowMemory (address, size, length) -> showMemory(address, size, length)
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
                Thread.Sleep(700)

    let debuggerLoop() =
        while _state.ProcessingCommands do
            match readCommands() |> parseSingleCommandString with
            | NoCommand -> 
                // repeat the previous command            
                parseCommandString(_lastCommandString) 
            | c -> 
                _state.LastCommand <- c
                parseCommand()        
            
        _waitEvent.Set()

    let writeDisassembly(proc: IProcessContainer) =
        let instruction = ES.Sojobo.Utility32.disassemble(proc, proc.GetInstruction())
        let pc = proc.ProgramCounter.Value |> BitVector.toUInt64
        match _comments.TryGetValue(pc) with
        | (true, text) -> String.Format("{0} ; {1}", instruction, text)
        | _ -> instruction
        |> Console.WriteLine

    let writeIR(proc: IProcessContainer) =
        ES.Sojobo.Utility32.disassembleCurrentInstructionIR(proc)
        |> Array.iter(Console.WriteLine)

    let processBreakPointCommand(proc: IProcessContainer) =        
        match _breakpoints.TryGetValue(proc.GetInstruction().Address) with
        | (true, bp) -> bp.Command |> Option.iter(parseCommandString)
        | _ -> ()

    let removeStepHook() =
        match _state.StepAddress with
        | Some address -> removeBreakpoint(address)
        | _ -> ()

    member val PrintDisassembly = false with get, set
    member val PrintIR = false with get, set

    member this.BeforeEmulation() =
        let proc = sandbox.GetRunningProcess()        
        
        // print disassembly
        if this.PrintDisassembly || _state.TracingMode then 
            writeDisassembly(proc)
        elif _state.ForcePrint then
            _state.ForcePrint <- false
            writeDisassembly(proc)

        if this.PrintIR then writeIR(proc)

        _state.InstructionToEmulate <- proc.GetInstruction() |> Some
        
        // check if must enter debugger loop
        if _state.IsInInteractiveMode() then
            removeStepHook()
            processBreakPointCommand(proc)
            _state.EnterDebuggerLoop()            
            debuggerLoop()

    member this.Break() =
        _state.Break()
            
    member this.Start() = 
        ignore (async { readBreakCommand() } |> Async.StartAsTask)
