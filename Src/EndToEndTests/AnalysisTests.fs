namespace ES.EndToEndTests

open System
open System.Collections.Generic
open ES.Sojobo

module AnalysisTests =

    let ``graph generation``() =
    (*
        let sandbox = new Win32Sandbox() 
        let exe = Utility.getTestFile()
        sandbox.Load(exe)

        // setup handlers
        let mutable counter = 0
        let mutable stackFrameCounter = 1
        let data = new Dictionary<Int32, Int32>()
        let proc = sandbox.GetRunningProcess()
        proc.Step.Add(fun proc ->
            counter <- counter + 1
            ES.EndToEndTests.Utility.writeDisassembly(proc)
            let instruction = proc.GetInstruction()
            if instruction.IsCall() then
                stackFrameCounter <- stackFrameCounter + 1
                data.Add(counter, stackFrameCounter)
            elif instruction.IsRET() then
                stackFrameCounter <- stackFrameCounter - 1
                data.Add(counter, stackFrameCounter)

            if counter > 10000 then
                sandbox.Stop()
        )

        // run the sample
        sandbox.Run()

        // create CSV to be plotted (Eg. on https://plot.ly/create/#/)
        Console.WriteLine("#Instruction, #Stack frame")
        for kv in data do
            Console.WriteLine("{0},{1}", kv.Key, kv.Value)
            *)
        Console.WriteLine("DONE")