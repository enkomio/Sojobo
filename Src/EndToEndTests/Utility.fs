namespace ES.EndToEndTests

open System
open System.IO
open B2R2
open ES.Sojobo

module internal Utility =
    let isInRange(activeProcess: IProcessContainer, startAddr: UInt64, endAddr: UInt64) =
        let pc = activeProcess.ProgramCounter.Value |> BitVector.toUInt64
        pc >= startAddr && pc <= endAddr

    let debuggerBreak(activeProcess: IProcessContainer) =
        ["EAX"; "EBX"; "ECX"; "EDX"; "ESI"; "EDI"]
        |> List.iter(fun register ->
            let address = activeProcess.GetRegister(register).Value |> BitVector.toUInt64
            let region =
                if activeProcess.Memory.IsAddressMapped(address)
                then activeProcess.Memory.GetMemoryRegion(address).BaseAddress
                else 0UL
            Console.WriteLine("{0}=[{1}]:{2}", register, region, address)            
        )
        Console.ReadLine() |> ignore

    let writeDisassembly(activeProcess: IProcessContainer) =
        let text = ES.Sojobo.Utility.formatCurrentInstruction(activeProcess)
        Console.WriteLine(text)

    let writeIR(activeProcess: IProcessContainer) =
        ES.Sojobo.Utility.formatCurrentInstructionIR(activeProcess)
        |> Array.iter(Console.WriteLine)

    let getTestFile() =
        ["Release"; "Debug"]
        |> Seq.map(fun dir -> Path.Combine("..", "..", "..", dir, "RunShellcodeWithVirtualAlloc.exe"))
        |> Seq.tryFind(File.Exists)
        |> function
            | Some exe -> exe
            | None ->
                Console.WriteLine("RunShellcodeWithVirtualAlloc.exe not found, please compile it first!")
                Environment.Exit(1)
                String.Empty