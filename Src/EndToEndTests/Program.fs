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

    let getSystemTimeAsFileTime(win32Process: Win32ProcessContainer) =
        Console.WriteLine("getSystemTimeAsFileTime")

    let getCurrentThreadId(win32Process: Win32ProcessContainer) =
        Console.WriteLine("getCurrentThreadId")

    let getCurrentProcessId(win32Process: Win32ProcessContainer) =
        Console.WriteLine("getCurrentProcessId")

    let queryPerformanceCounter(win32Process: Win32ProcessContainer) =
        Console.WriteLine("queryPerformanceCounter")

    [<EntryPoint>]
    let main argv =
        let settings = 
            {defaultSandboxConfig with
                PrintAssembly = true
                PrintIR = false
            }
        let sandbox = new Win32Sandbox(settings)
        //sandbox.Run(getBytes())
        
        sandbox.AddCallback("GetSystemTimeAsFileTime", "KERNEL32.dll", new Action<Win32ProcessContainer>(getSystemTimeAsFileTime))
        sandbox.AddCallback("GetCurrentThreadId", "KERNEL32.dll", new Action<Win32ProcessContainer>(getCurrentThreadId))
        sandbox.AddCallback("GetCurrentProcessId", "KERNEL32.dll", new Action<Win32ProcessContainer>(getCurrentProcessId))
        sandbox.AddCallback("QueryPerformanceCounter", "KERNEL32.dll", new Action<Win32ProcessContainer>(queryPerformanceCounter))
                
        let unShellcodeWithVirtualAlloc = Path.Combine("..", "..", "..", "Debug", "RunShellcodeWithVirtualAlloc.exe")
        sandbox.Run(unShellcodeWithVirtualAlloc)
        0
