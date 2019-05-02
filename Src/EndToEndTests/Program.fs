namespace EndToEndTests

open System
open System.IO
open B2R2
open ES.Sojobo
open ES.Sojobo.Model

module Program =

    let step(activeProcess: IProcessContainer) =
        let text = Utility.formatCurrentInstruction(activeProcess)
        Console.WriteLine(text)

        let printUI = false
        if printUI then
            Utility.formatCurrentInstructionIR(activeProcess)
            |> Array.iter(Console.WriteLine)

    [<EntryPoint>]
    let main argv =
        let sandbox = new Win32Sandbox()   
        
        (*
        TODO implementare la logica con Lib (Kernel32Lib).
        Implementare un metodo che permetta di sovrascrivere le funzioni (eg. AddAssembly resolver)
        Da mettere in sandbox class
        Return deve pulire lo stack a seconda del tipo di chiamata (info da mettere nel risultato)
        *)

        (*
        sandbox.AddCallback("GetSystemTimeAsFileTime", "KERNEL32.dll", new Action<IProcessContainer>(getSystemTimeAsFileTime))
        sandbox.AddCallback("GetCurrentThreadId", "KERNEL32.dll", new Action<IProcessContainer>(getCurrentThreadId))
        sandbox.AddCallback("GetCurrentProcessId", "KERNEL32.dll", new Action<IProcessContainer>(getCurrentProcessId))
        sandbox.AddCallback("QueryPerformanceCounter", "KERNEL32.dll", new Action<IProcessContainer>(queryPerformanceCounter))
        sandbox.AddCallback("IsProcessorFeaturePresent", "KERNEL32.dll", new Action<IProcessContainer>(isProcessorFeaturePresent))

        sandbox.AddCallback("memset", "VCRUNTIME140.dll", new Action<IProcessContainer>(memset))
          *)
          
        let unShellcodeWithVirtualAlloc = Path.Combine("..", "..", "..", "Release", "RunShellcodeWithVirtualAlloc.exe")
        
        // create and run the process
        sandbox.Create(unShellcodeWithVirtualAlloc)
        let proc = sandbox.GetRunningProcess()
        
        // print imported function
        proc.GetImportedFunctions()
        |> Seq.iter(fun symbol ->
            Console.WriteLine("Import: [0x{0}] {1} ({2}) from {3}", symbol.Address.ToString("X"), symbol.Name, symbol.Kind, symbol.LibraryName)            
        )
        
        proc.Step.Add(step)
        sandbox.Run()
        0
