namespace EndToEndTests

open System
open System.IO
open B2R2
open ES.Sojobo
open ES.Sojobo.Model

module Program =
    let mutable print = false
    let step(activeProcess: IProcessContainer) =
        let ip = activeProcess.GetProgramCounter().Value |> BitVector.toInt32
        if ip = 0x40144a then            
            //print <- true
            ()

        let text = Utility.formatCurrentInstruction(activeProcess)
        Console.WriteLine(text)       

        if print then
            Console.WriteLine("Register before execution")
            Console.WriteLine("EAX=" + activeProcess.GetVariable("EAX").Value.ToString())
            Console.WriteLine("ECX=" + activeProcess.GetVariable("ECX").Value.ToString())
            Console.WriteLine("EDX=" + activeProcess.GetVariable("EDX").Value.ToString())
            Console.WriteLine("ESI=" + activeProcess.GetVariable("ESI").Value.ToString())
            Console.WriteLine("EDI=" + activeProcess.GetVariable("EDI").Value.ToString())
            Console.ReadKey() |> ignore
            
        let printUI = false
        if printUI then
            Utility.formatCurrentInstructionIR(activeProcess)
            |> Array.iter(Console.WriteLine)

    [<EntryPoint>]
    let main argv =
        let sandbox = new Win32Sandbox()   
        
        let runShellcodeWithVirtualAlloc = Path.Combine("..", "..", "..", "Release", "RunShellcodeWithVirtualAlloc.exe")
        
        // create and run the process
        sandbox.Load(runShellcodeWithVirtualAlloc)
        let proc = sandbox.GetRunningProcess()
        
        // print imported function
        proc.GetImportedFunctions()
        |> Seq.iter(fun symbol ->
            Console.WriteLine("Import: [0x{0}] {1} ({2}) from {3}", symbol.Address.ToString("X"), symbol.Name, symbol.Kind, symbol.LibraryName)            
        )
        
        proc.Step.Add(step)
        sandbox.Run()
        0
