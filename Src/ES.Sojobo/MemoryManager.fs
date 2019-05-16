namespace ES.Sojobo

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Reflection
open B2R2
open B2R2.FrontEnd
open ES.Sojobo.Model

type private Patch = {
    Offset: Int32
    Source: Object
    SourceContent: Byte array
    Field: Byte array
}

type MemoryManager(pointerSize: Int32) =
    let _va = new Dictionary<UInt64, MemoryRegion>() 
    let _memoryAccessEvent = new Event<MemoryAccessOperation>()

    let _stack =
        let stack = 
            createMemoryRegion(
                0x18C000UL, 
                0x4000, 
                MemoryProtection.Read ||| MemoryProtection.Write
            )
        _va.Add(stack.BaseAddress, stack)
        stack

    let _heap =
        let heap = 
            createMemoryRegion(
                0x520000UL, 
                0x16000,
                MemoryProtection.Read ||| MemoryProtection.Write
            )
        _va.Add(heap.BaseAddress, heap)
        heap
    
    let rec serialize(value: Object, patches: List<Patch>) =
        // serialize object in memory
        let size = Marshal.SizeOf(value)
        let ptr = Marshal.AllocHGlobal(size)
        Marshal.StructureToPtr(value, ptr, true)

        // write content and free buffer
        let buffer = Array.zeroCreate<Byte>(size)
        Marshal.Copy(ptr, buffer, 0, size)
        Marshal.FreeHGlobal(ptr)

        // serialize the fields that are not primitive types
        serializeFields(value, buffer, patches)

        buffer

    and serializeFields(value: Object, serializedValue: Byte array, patches: List<Patch>) =
        // write to buffer not primitive type
        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public        
        value.GetType().GetFields(flags)
        |> Array.filter(fun field -> field.GetValue(value) <> null)
        |> Array.iter(fun field ->            
            let fieldValue = field.GetValue(value)
            let offset = Marshal.OffsetOf(value.GetType(), field.Name).ToInt32()
                        
            // serialize field if necessary
            match patches |> Seq.tryFind(fun p -> Object.ReferenceEquals(p.Source, fieldValue)) with
            | Some patch -> 
                let newPatch = {patch with Offset = offset}
                patches.Add(newPatch)
            | None ->                    
                if field.FieldType.IsArray then
                    // TODO: implements it
                    ()
                elif field.FieldType.IsClass then
                    let fieldSerializedBuffer = serialize(fieldValue, patches)
                    patches.Add({
                        Offset = offset
                        Source = value
                        SourceContent = serializedValue
                        Field = fieldSerializedBuffer
                    })
        )

    member this.MemoryAccess = _memoryAccessEvent.Publish  
    member val Stack = _stack with get, set
    member val Heap = _heap with get, set
    
    member this.ReadMemory(address: UInt64, size: Int32) =
        // TODO: add check on memory protection
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Read address)
        let memRegion = this.GetMemoryRegion(address)
        BinHandler.ReadBytes(memRegion.Handler, address, size)

    member internal this.UnsafeWriteMemory(address: UInt64, value: Byte array, verifyProtection: Boolean) =        
        let region = this.GetMemoryRegion(address)
        if verifyProtection then    
            // TODO: add check on memory protection
            ()

        let offset = region.Handler.FileInfo.TranslateAddress address
        Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)

    member this.WriteMemory(address: UInt64, value: Byte array) =
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Write (address, value))
        this.UnsafeWriteMemory(address, value, true)

    member this.WriteMemory(address: UInt64, value: Object) =
        // serialize object
        let patches = new List<Patch>()
        let sourceBuffer = serialize(value, patches)

        // apply patch
        let totalSize = patches |> Seq.sumBy(fun p -> p.Field.Length)
        let mutable fieldsMemRegionAddr = this.AllocateMemory(totalSize, this.GetMemoryRegion(address).Protection)
        patches
        |> Seq.iter(fun patch ->
            // write the content of the field
            this.UnsafeWriteMemory(fieldsMemRegionAddr, patch.Field, false)

            // write the address
            let fieldsMemRegionAddrBytes =
                if pointerSize = 32
                then BitConverter.GetBytes(uint32 fieldsMemRegionAddr)
                else BitConverter.GetBytes(fieldsMemRegionAddr)
                            
            Array.Copy(fieldsMemRegionAddrBytes, 0, patch.SourceContent, patch.Offset, fieldsMemRegionAddrBytes.Length)
            fieldsMemRegionAddr <- fieldsMemRegionAddr + uint64 patch.Field.Length
        )

        // write content of the main object
        this.UnsafeWriteMemory(address, sourceBuffer, false)

    member this.UpdateMemoryRegion(baseAddress: UInt64, memoryRegion: MemoryRegion) =
        _va.[baseAddress] <- memoryRegion

    member this.GetMemoryRegion(address: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address < endAddr
        )

    member this.AddMemoryRegion(memRegion: MemoryRegion) =
        _va.[memRegion.BaseAddress] <- memRegion

    member this.GetMemoryMap() =
        _va.Values 
        |> Seq.sortBy(fun m -> m.BaseAddress)
        |> Seq.readonly 
        |> Seq.toArray

    member this.FreeMemoryRegion(address: UInt64) =        
        let region = this.GetMemoryRegion(address)
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Free region)
        _va.Remove(region.BaseAddress)        

    member this.AllocateMemory(size: Int32, protection: MemoryProtection) =
        let baseAddress =
            this.GetMemoryMap()
            |> Seq.pairwise
            |> Seq.tryFind(fun (m1, m2) ->
                let availableSize = m2.BaseAddress - (m1.BaseAddress + uint64 m1.Content.Length)
                availableSize > uint64 size
            )
            |> function
                | Some (m1, _) -> 
                    m1.BaseAddress + uint64 m1.Content.Length
                | None -> 
                    let lastRegion = this.GetMemoryMap() |> Array.last
                    lastRegion.BaseAddress + uint64 lastRegion.Content.Length

        // create the memory region
        let region = createMemoryRegion(baseAddress, size, protection)
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Allocate region)
        this.AddMemoryRegion(region)

        baseAddress

    member this.AllocateMemory(value: Byte array, protection: MemoryProtection) =        
        let baseAddress = this.AllocateMemory(value.Length, protection)
        this.WriteMemory(baseAddress, value)
        baseAddress

    member this.AllocateMemory(value: Object, protection: MemoryProtection) =
        let size = Marshal.SizeOf(value)
        let baseAddress = this.AllocateMemory(size, protection)
        this.WriteMemory(baseAddress, value)
        baseAddress