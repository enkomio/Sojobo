namespace EndToEndTests

open System
open System.IO
open ES.Sojobo.Win32Sandbox

module Program =    
    let getBytes() =
        (*
        .text:00374753 55                                            push    ebp
        .text:00374754 8B EC                                         mov     ebp, esp
        .text:00374756 51                                            push    ecx
        .text:00374757 8B 0D 08 19 3A 00                             mov     ecx, dword ptr qword_3A1908
        .text:0037475D A1 0C 19 3A 00                                mov     eax, dword ptr qword_3A1908+4
        .text:00374762 33 D2                                         xor     edx, edx
        .text:00374764 83 C1 3C                                      add     ecx, 3Ch ; '<'
        *)
        [|
            0x55; 0x8B; 0xEC; 0x51; 0x8B; 0x0D; 0x08; 0x19; 0x3A; 
            0x00; 0xA1; 0x0C; 0x19; 0x3A; 0x00; 0x33; 0xD2; 0x83; 
            0xC1; 0x3C

        |]
        |> Array.map byte

    [<EntryPoint>]
    let main argv =    
        let settings = 
            {defaultSandboxConfig with
                PrintAssembly = true
                PrintIR = true
            }
        let sandbox = new Win32Sandbox(settings)
        sandbox.Run(getBytes())
        //sandbox.Run(@"c:\install.exe")
        0
