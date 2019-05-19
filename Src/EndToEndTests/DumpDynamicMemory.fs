namespace ES.EndToEndTests

open System
open System.IO
open System.Collections.Generic
open B2R2
open ES.Sojobo.Model
open ES.Sojobo

module DumpDynamicMemory =
    let private _memoryRegions = new List<MemoryRegion>()
    let mutable private _memoryDumped = false

    let private memoryAccessedHandler(operation: MemoryAccessOperation) =
        match operation with
        | Read address -> ()
        | Write(address, value) -> ()
        | Allocate memRegion -> _memoryRegions.Add(memRegion)
        | Free memRegion -> ()

    let private writeDisassembly(activeProcess: IProcessContainer) =
        let text = Utility.formatCurrentInstruction(activeProcess)
        Console.WriteLine(text)

    let private identifyUnpackedCode(activeProcess: IProcessContainer) =
        if not _memoryDumped then
            let pc = activeProcess.GetProgramCounter().Value |> BitVector.toUInt32
            _memoryRegions
            |> Seq.tryFind(fun memRegion -> 
                pc >= uint32 memRegion.BaseAddress &&
                pc < uint32 memRegion.BaseAddress + uint32 memRegion.Content.Length
            )
            |> Option.iter(fun memRegion ->
                // a previously allocated region now is being executed, maybe unpacked code!            
                let filename = String.Format("mem_{0}.bin", memRegion.BaseAddress)
                File.WriteAllBytes(filename, memRegion.Content)
                Console.WriteLine("[+] Dynamic code dumped to: {0}!", filename)
                _memoryDumped <- true
            )

    let private step(activeProcess: IProcessContainer) =
        writeDisassembly(activeProcess)
        identifyUnpackedCode(activeProcess)

    let private getTestFile() =
        ["Release"; "Debug"]
        |> Seq.map(fun dir -> Path.Combine("..", "..", "..", dir, "RunShellcodeWithVirtualAlloc.exe"))
        |> Seq.tryFind(File.Exists)

    let ``dump dynamically executed memory``() =
        let sandbox = new Win32Sandbox() 
        let exe = 
            match getTestFile() with
            | Some exe -> exe
            | None ->
                Console.WriteLine("RunShellcodeWithVirtualAlloc.exe not found, please compile it first!")
                Environment.Exit(1)
                String.Empty

        sandbox.Load(exe)

        // setup handlers
        let proc = sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(memoryAccessedHandler)
        proc.Step.Add(step)
        
        // print imported function
        proc.GetImportedFunctions()
        |> Seq.iter(fun symbol ->
            Console.WriteLine(
                "Import: [0x{0}] {1} ({2}) from {3}", 
                symbol.Address.ToString("X"), 
                symbol.Name, 
                symbol.Kind, 
                symbol.LibraryName
            )            
        )
        
        // run the sample
        sandbox.Run()