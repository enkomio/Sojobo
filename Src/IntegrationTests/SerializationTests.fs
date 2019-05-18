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
        let d1 = Activator.CreateInstance<ListEntryData>()
        let d2 = Activator.CreateInstance<ListEntryData>()

        // set link to refer to itself
        d1.Forward <- Activator.CreateInstance<LIST_ENTRY_FORWARD>()
        d1.Backward <- Activator.CreateInstance<LIST_ENTRY_BACKWARD>()

        d2.Forward <- Activator.CreateInstance<LIST_ENTRY_FORWARD>()        
        d2.Backward <- Activator.CreateInstance<LIST_ENTRY_BACKWARD>()

        d1.Forward.Flink <- d2.Forward
        d1.Backward.Blink <- d2.Backward

        d2.Forward.Flink <- d1.Forward
        d2.Backward.Blink <- d1.Backward
        
        // serialize the data
        let testData1 = {D1 = d1; D2 = d2}
        let memManager = new MemoryManager(32)
        let addr = memManager.AllocateMemory(testData1, MemoryProtection.Read)

        // unserialize the object
        let testData2 = memManager.ReadMemory<TestData>(addr)
        assert(testData2.D1.Forward.Flink.Flink = testData2.D1.Forward)

