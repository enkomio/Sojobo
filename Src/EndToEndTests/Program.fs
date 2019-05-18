namespace EndToEndTests

open System
open System.Collections.Generic
open System.IO
open B2R2
open ES.Sojobo
open ES.Sojobo.Model

module Program =
    open System.Text

    type Placeholder() =
        do ()

    let mutable print = false
    let mutable printUI = false
    let _memoryRegions = new List<MemoryRegion>()

    let writeDisassembly(activeProcess: IProcessContainer) =
        let ip = activeProcess.GetProgramCounter().Value |> BitVector.toInt32
        let ips = [
            0x406936
            //0x406966
        ]
        if ips |> List.contains ip then            
            print <- true
            //printUI <- true
            ()

        let text = Utility.formatCurrentInstruction(activeProcess)
        Console.WriteLine(text)       

        if print then
            (*let eax = activeProcess.GetRegister("EAX").Value |> BitVector.toInt32
            let edi = activeProcess.GetRegister("EDI").Value |> BitVector.toUInt64
            let buffer = activeProcess.Memory.ReadMemory(edi, eax)
            let s = Encoding.UTF8.GetString(buffer)
            Console.WriteLine(s)
            *)
            Console.WriteLine("Register before execution")
            Console.WriteLine("EAX=" + activeProcess.GetRegister("EAX").Value.ToString())
            Console.WriteLine("ECX=" + activeProcess.GetRegister("ECX").Value.ToString())
            Console.WriteLine("EDX=" + activeProcess.GetRegister("EDX").Value.ToString())
            Console.WriteLine("ESI=" + activeProcess.GetRegister("ESI").Value.ToString())
            Console.WriteLine("EDI=" + activeProcess.GetRegister("EDI").Value.ToString())
            Console.WriteLine("FS=" + activeProcess.GetRegister("FS").Value.ToString())
            Console.WriteLine("FSBase=" + activeProcess.GetRegister("FSBase").Value.ToString())
            Console.ReadKey() |> ignore            
        
        if printUI then
            Utility.formatCurrentInstructionIR(activeProcess)
            |> Array.iter(Console.WriteLine)

    let identifyUnpackedCode(activeProcess: IProcessContainer) =
        let pc = activeProcess.GetProgramCounter().Value |> BitVector.toUInt32
        _memoryRegions
        |> Seq.tryFind(fun memRegion -> 
            pc >= uint32 memRegion.BaseAddress &&
            pc < uint32 memRegion.BaseAddress + uint32 memRegion.Content.Length
        )
        |> Option.iter(fun memRegion ->
            // a previously allocated region now is being executed, maybe unpacked code!
            let name = String.Format("mem_{0}.bin", memRegion.BaseAddress)
            File.WriteAllBytes(name, memRegion.Content)
        )

    let step(activeProcess: IProcessContainer) =
        writeDisassembly(activeProcess)
        identifyUnpackedCode(activeProcess)

    let memoryAccessedHandler(operation: MemoryAccessOperation) =
        match operation with
        | Read address -> ()
        | Write(address, value) -> ()
        | Allocate memRegion -> _memoryRegions.Add(memRegion)
        | Free memRegion -> ()

    let getMalware() =
        let path = @"C:\Workspace\Sojobo\Malware\kpot2.0_safe.txt"
        let content = File.ReadAllBytes(path)
        content.[0] <- byte 'M'
        content.[1] <- byte 'Z'
        content

    [<EntryPoint>]
    let main argv =
        let sandbox = new Win32Sandbox() 
        let exe = 
            if argv.Length > 0
            then argv.[0]
            else Path.Combine("..", "..", "..", "Release", "RunShellcodeWithVirtualAlloc.exe")
        
        // create and run the process
        //sandbox.Load(exe)
        sandbox.Load(getMalware())
        sandbox.AddLibrary((new Placeholder()).GetType().Assembly)

        let proc = sandbox.GetRunningProcess()
        proc.Memory.MemoryAccess.Add(memoryAccessedHandler)
                
        // print imported function
        proc.GetImportedFunctions()
        |> Seq.iter(fun symbol ->
            Console.WriteLine("Import: [0x{0}] {1} ({2}) from {3}", symbol.Address.ToString("X"), symbol.Name, symbol.Kind, symbol.LibraryName)            
        )

        proc.Step.Add(step)
        sandbox.Run()
        0
