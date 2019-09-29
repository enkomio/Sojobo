namespace ES.EndToEndTests

open B2R2
open ES.Sojobo.Model
open ES.Sojobo
open ES.Sojobo.Windows

module SnapshotTests =

    let ``test snapshot creation and loading``() =
        let sandbox = new Win32Sandbox() 
        let mutable snapshot: Snapshot option = None
        let snapshotManager = new SnapshotManager(sandbox)
        let exe = Utility.getTestFile()
        sandbox.Load(exe)
        
        // setup handlers
        sandbox.BeforeEmulation.Add(fun proc ->
            ES.EndToEndTests.Utility.writeDisassembly(proc)

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