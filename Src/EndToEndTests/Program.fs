namespace EndToEndTests

open System
open System.IO
open ES.Sojobo.Win32Sandbox

module Program =    
    open System.Diagnostics

    let getBytes() =
        [|
            0x25; 0x00; 0x00; 0xFF; 0xFF
        |]
        |> Array.map byte

    [<EntryPoint>]
    let main argv =
        let settings = 
            {defaultSandboxConfig with
                PrintAssembly = true
                PrintIR = false
            }
        let sandbox = new Win32Sandbox(settings)
        //sandbox.Run(getBytes())

        let unShellcodeWithVirtualAlloc = Path.Combine("..", "..", "..", "Debug", "RunShellcodeWithVirtualAlloc.exe")
        sandbox.Run(unShellcodeWithVirtualAlloc)
        0
