namespace EndToEndTests

open System
open System.IO
open ES.Sojobo.Win32Sandbox
open System.Diagnostics
open ES.Sojobo

module Program =    
   
    let getBytes() =
        [|
            0x25; 0x00; 0x00; 0xFF; 0xFF
        |]
        |> Array.map byte

    let emulateGetSystemTimeAsFileTime(win32Process: Win32ProcessContainer) =
        Console.WriteLine("AAAAA")
        ()

    [<EntryPoint>]
    let main argv =
        let settings = 
            {defaultSandboxConfig with
                PrintAssembly = true
                PrintIR = false
            }
        let sandbox = new Win32Sandbox(settings)
        //sandbox.Run(getBytes())
        sandbox.AddCallback("GetSystemTimeAsFileTime", "KERNEL32.dll", new Action<Win32ProcessContainer>(emulateGetSystemTimeAsFileTime))
        let unShellcodeWithVirtualAlloc = Path.Combine("..", "..", "..", "Debug", "RunShellcodeWithVirtualAlloc.exe")
        sandbox.Run(unShellcodeWithVirtualAlloc)
        0
