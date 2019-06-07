namespace ES.EndToEndTests

open System
open System.IO
open System.Collections.Generic
open B2R2
open ES.Sojobo.Model
open ES.Sojobo

module SnapshotTests =

    let ``test snapshot creation and loading``() =
        let sandbox = new Win32Sandbox() 
        let mutable snapshot: Snapshot option = None
        let snapshotManager = new SnapshotManager(sandbox)
        let exe = Utility.getTestFile()
        sandbox.Load(exe)
        
        // setup handlers
        let proc = sandbox.GetRunningProcess()
        proc.Step.Add(fun proc ->
            ES.EndToEndTests.Utility.writeDisassembly(proc)
            //ES.EndToEndTests.Utility.writeIR(proc)

            (*
            if Utility.isInRange(proc, 0x19145DUL, 0x191470UL) then
                Utility.debuggerBreak(proc)
            *)

            if 0x401061 = (proc.ProgramCounter.Value |> BitVector.toInt32) then
                // just finished to compute fibonacci
                snapshot <- Some <| snapshotManager.TakeSnaphot()
        )

        // run the sample
        sandbox.Run()

        // load the snapshot to a new sanbox
        let sandbox = new Win32Sandbox() 
        let snapshotManager = new SnapshotManager(sandbox)
        snapshotManager.LoadSnapshot(snapshot.Value)

        // verify
        let eax = sandbox.GetRunningProcess().Cpu.GetRegister("EAX").Value |> BitVector.toInt32
        assert(eax = 8)