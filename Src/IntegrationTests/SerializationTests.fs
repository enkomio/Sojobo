namespace IntegrationTests

open System
open ES.Sojobo
open ES.Sojobo.Win32
open ES.Sojobo.Model
open System.Runtime.InteropServices

[<CLIMutable>]
[<ReferenceEquality>]
[<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type ListEntryData = {
    mutable Forward: LIST_ENTRY_FORWARD
    mutable Backward: LIST_ENTRY_BACKWARD
}

[<CLIMutable>]
[<ReferenceEquality>]
[<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
type TestData = {
    mutable D1: ListEntryData
    mutable D2: ListEntryData
}

module SerializationTests =

    let ``Serialize auto-referenced object``() =
        let t1 = Activator.CreateInstance<ListEntryData>()
        let t2 = Activator.CreateInstance<ListEntryData>()

        // set link to refer to itself
        t1.Forward <- Activator.CreateInstance<LIST_ENTRY_FORWARD>()
        t1.Backward <- Activator.CreateInstance<LIST_ENTRY_BACKWARD>()

        t2.Forward <- Activator.CreateInstance<LIST_ENTRY_FORWARD>()
        t2.Forward.Flink <- t1.Forward
        t2.Backward <- Activator.CreateInstance<LIST_ENTRY_BACKWARD>()
        t2.Backward.Blink <- t2.Backward

        t1.Forward.Flink <- t2.Forward
        t1.Backward.Blink <- t2.Backward

        let d = {D1 = t1; D2 = t2}
        let memManager = new MemoryManager(32)
        let addr = memManager.AllocateMemory(d, MemoryProtection.Read)
        let content = memManager.GetMemoryRegion(addr).Content 
        ()

