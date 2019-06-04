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

    let private identifyUnpackedCode(activeProcess: IProcessContainer) =
        if not _memoryDumped then
            let pc = activeProcess.ProgramCounter.Value |> BitVector.toUInt32
            _memoryRegions
            |> Seq.tryFind(fun memRegion -> 
                pc >= uint32 memRegion.BaseAddress &&
                pc < uint32 memRegion.BaseAddress + uint32 memRegion.Content.Length
            )
            |> Option.iter(fun memRegion ->
                // a previously allocated region now is being executed, maybe unpacked code!            
                let filename = String.Format("mem_dump_{0}_via_memory_check.bin", memRegion.BaseAddress)
                File.WriteAllBytes(filename, memRegion.Content)
                Console.WriteLine("[+] Dynamic code dumped to: {0}!", filename)
                _memoryDumped <- true
            )

    let private step(activeProcess: IProcessContainer) =
        Utility.writeDisassembly(activeProcess)
        identifyUnpackedCode(activeProcess)

    let ``dump freed memory by using hooks``() =
        let mutable memoryDumped = false
        let sandbox = new Win32Sandbox() 
        let exe = Utility.getTestFile()
        sandbox.Load(exe)

        // add kernel32 in order to place the hook correctly
        sandbox.AddLibrary(@"C:\Windows\SysWOW64\Kernel32.dll");
        
        // hook callback that will dump the memory region
        let hookCallback(sandbox: ISandbox) =
            // we are at tha start of the function, on top
            // of the stack there is the return value. The stack
            // frame is not yet created
            let proc = sandbox.GetRunningProcess()
            let esp = proc.Cpu.GetRegister("ESP").Value |> BitVector.toUInt64
            
            // get address first argument by skipping return address
            let addrRegionToFree = proc.Memory.ReadMemory<UInt32>(esp + 4UL)
            let memRegion = proc.Memory.GetMemoryRegion(uint64 addrRegionToFree)

            // dump freed memory
            let filename = String.Format("mem_dump_{0}_via_VirtualFree_hook.bin", memRegion.BaseAddress)
            File.WriteAllBytes(filename, memRegion.Content)
            Console.WriteLine("[+] Dynamic code dumped to: {0}!", filename)
            memoryDumped <- true

        // add hook on VirtualFree
        sandbox.AddHook("kernel32!VirtualFree", new Action<ISandbox>(hookCallback))

        // setup handlers
        let proc = sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(memoryAccessedHandler)
        proc.Step.Add(Utility.writeDisassembly)

        // run the sample
        sandbox.Run()

        // verify memory was dumper
        assert(memoryDumped)

    let ``dump dynamically executed memory``() =
        let sandbox = new Win32Sandbox() 
        let exe = Utility.getTestFile()
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