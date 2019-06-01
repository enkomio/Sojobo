namespace ES.Sojobo

open System
open B2R2
open B2R2.BinFile
open ES.Sojobo.Model

type SnapshotManager(sandbox: BaseSandbox) =
    member this.TakeSnaphot() =
        let mutable stackMemoryRegionId = Guid.Empty
        let mutable heapMemoryRegionId = Guid.Empty

        let addressSpace =
            sandbox.GetRunningProcess().Memory.GetMemoryMap()
            |> Array.map(fun memRegion -> 
                let regionSnapshot =  {
                    Id = Guid.NewGuid()
                    BaseAddress = memRegion.BaseAddress
                    Content = memRegion.Content |> Array.copy
                    Permission = int32 memRegion.Permission
                    Type = memRegion.Type
                    Info = memRegion.Info
                }

                // identify stack and heap regions
                if regionSnapshot.BaseAddress = sandbox.GetRunningProcess().Memory.Stack.BaseAddress then
                    stackMemoryRegionId <- regionSnapshot.Id
                elif regionSnapshot.BaseAddress = sandbox.GetRunningProcess().Memory.Heap.BaseAddress then
                    heapMemoryRegionId <- regionSnapshot.Id

                regionSnapshot
            )

        // create the snapshot object
        {
            Date = DateTime.UtcNow
            HeapRegionId = heapMemoryRegionId
            StackRegionId = stackMemoryRegionId
            VirtualAddressSpace = addressSpace
            Registers =
                match sandbox.GetRunningProcess() with
                | :? BaseProcessContainer as baseProcess ->
                    baseProcess.Variables
                    |> Seq.map(fun kv -> {
                        Name = kv.Key
                        Size = Helpers.getSize(kv.Value.Type)
                        Value = kv.Value.Value
                        IsTemp = kv.Value.IsTemp
                    })
                    |> Seq.toArray
                | _ -> Array.empty
        }

    member this.LoadSnapshot(snapshot: Snapshot) =
        // cleanup stuff
        sandbox.CreateEmptyProcess()
        let memory = sandbox.GetRunningProcess().Memory
        memory.Clear()        

        // setup Virtual Address Space
        snapshot.VirtualAddressSpace
        |> Array.iter(fun memRegion ->
            // create region
            memory.AllocateMemory(
                memRegion.BaseAddress, 
                memRegion.Content.Length, 
                (Enum.Parse(typeof<Permission>, memRegion.Permission.ToString()) :?> Permission)
            ) |> ignore

            // update memory region with type and info
            let allocatedMemoryRegion =
                { memory.GetMemoryRegion(memRegion.BaseAddress) with
                    Type = memRegion.Type
                    Info = memRegion.Info
                } 
            memory.UpdateMemoryRegion(memRegion.BaseAddress, allocatedMemoryRegion)
            
            // write region content
            memory.UnsafeWriteMemory(memRegion.BaseAddress, memRegion.Content, false)

            // check for stack or heap region
            if memRegion.Id = snapshot.StackRegionId then
                memory.Stack <- allocatedMemoryRegion
            elif memRegion.Id = snapshot.HeapRegionId then
                memory.Heap <- allocatedMemoryRegion
        )

        // setup registers
        snapshot.Registers
        |> Array.map(fun register -> {
            Name = register.Name
            Value = register.Value
            IsTemp = register.IsTemp
            Type = Helpers.getType(register.Value |> BitVector.getType)
        })
        |> Array.iter(sandbox.GetRunningProcess().SetRegister)