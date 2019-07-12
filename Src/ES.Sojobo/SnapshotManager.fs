namespace ES.Sojobo

open System
open B2R2
open B2R2.BinFile
open ES.Sojobo.Model
open System.Collections.Generic

type SnapshotManager(sandbox: BaseSandbox) =
    member this.TakeSnaphot() =
        let mutable stackMemoryRegionId = Guid.Empty
        let mutable heapMemoryRegionId = Guid.Empty

        // read virtual address space
        let virtualAddressSpace =
            sandbox.GetRunningProcess().Memory.GetMemoryMap()
            |> Array.filter(fun region -> region.Content.Length > 0)
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
            ProcessId = sandbox.GetRunningProcess().Pid
            Date = DateTime.UtcNow
            HeapRegionId = heapMemoryRegionId
            StackRegionId = stackMemoryRegionId
            VirtualAddressSpace = virtualAddressSpace
            Registers =
                match sandbox.GetRunningProcess() with
                | :? BaseProcessContainer as baseProcess ->
                    baseProcess.Cpu.GetAllVariables()
                    |> Seq.map(fun kv -> {
                        Name = kv.Key
                        Size = Helpers.getSize(kv.Value.Type)
                        Value = kv.Value.Value
                        IsTemp = kv.Value.IsTemp
                    })
                    |> Seq.toArray
                | _ -> Array.empty
            Libraries = 
                getNativeLibraries(sandbox.Libraries)
                |> Array.filter(fun lib -> lib.Filename.IsSome)
                |> Array.map(fun lib -> {
                    Name = lib.GetLibraryName()
                    EntryPoint = lib.EntryPoint
                    Exports = 
                        new List<SymbolDto>(lib.Exports 
                            |> Seq.map(fun symbol -> {
                                Name = symbol.Name
                                Library = symbol.LibraryName
                                Address = symbol.Address
                        }))
                    BaseAddress = lib.BaseAddress                    
                })
        }

    member this.LoadSnapshot(snapshot: Snapshot) =
        // cleanup stuff
        sandbox.ResetProcessState()
        let proc = sandbox.GetRunningProcess(Pid=snapshot.ProcessId)
        let memory = proc.Memory

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
            memory.WriteMemory(memRegion.BaseAddress, memRegion.Content, false)

            // check for stack or heap region
            if memRegion.Id = snapshot.StackRegionId then
                memory.Stack <- allocatedMemoryRegion
            elif memRegion.Id = snapshot.HeapRegionId then
                memory.Heap <- allocatedMemoryRegion
        )

        // setup loaded libraries info
        snapshot.Libraries
        |> Array.iter(fun snapshotLib ->
            getNativeLibraries(sandbox.Libraries)
            |> Array.tryFind(fun lib -> lib.GetLibraryName().Equals(snapshotLib.Name, StringComparison.OrdinalIgnoreCase))
            |> function
                | Some lib ->                 
                    let symbols =
                        snapshotLib.Exports
                        |> Seq.map(fun sym -> {
                            Name = sym.Name
                            LibraryName = sym.Library
                            Address = sym.Address
                            Kind = SymbolKind.FunctionType
                            Target = TargetKind.DynamicSymbol
                        })
                    lib.SetProperties(snapshotLib.EntryPoint, snapshotLib.BaseAddress, new List<Symbol>(symbols))

                    // set symbols
                    symbols |> Seq.iter(proc.SetSymbol)
                | None -> ()
                
        )

        // setup registers
        snapshot.Registers
        |> Array.map(fun register -> {
            Name = register.Name
            Value = register.Value
            IsTemp = register.IsTemp
            Type = Helpers.getType(register.Value |> BitVector.getType)
        })
        |> Array.iter(sandbox.GetRunningProcess().Cpu.SetRegister)