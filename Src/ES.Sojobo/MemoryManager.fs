namespace ES.Sojobo

open System
open System.Collections
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Reflection
open System.IO
open System.Text
open B2R2
open B2R2.FrontEnd
open ES.Sojobo.Model
open B2R2.BinFile
open ES.Sojobo.Helpers

type private Fixup = {
    mutable Offset: Int64
    mutable ReferencedField: Object
    mutable This: Object
}

type private MemoryEntry = {
    Object: Object
    Buffer: Byte array
}

type MemoryManager(pointerSize: Int32) as this =
    let _va = new SortedDictionary<UInt64, MemoryRegion>() 
    let _memoryAccessEvent = new Event<MemoryAccessOperation>()
    let _memoryAccessViolation = new Event<MemoryAccessOperation>()
    let mutable _lastAllocatedLibBase = 0UL

    let createStack() =
        let stack = {
            createMemoryRegion(0x18C000UL, 0x4000, Permission.Readable ||| Permission.Writable) 
            with 
                Type = "Stack"
                Info = String.Empty
        }
        _va.Add(stack.BaseAddress, stack)
        stack

    let createHeap() =
        let heap = {
            createMemoryRegion(0x520000UL, 0x16000, Permission.Readable ||| Permission.Writable) 
            with 
                Type = "Heap"
                Info = String.Empty
            }
        _va.Add(heap.BaseAddress, heap)
        heap

    let getMemoryRegion(address: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address < endAddr
        )
        
    let rec serialize(value: Object, entries: List<MemoryEntry>, addEntry: Boolean, fixups: List<Fixup>, analyzedObjects: HashSet<Object>): (Byte array * List<Fixup>) =
        // allocate buffer
        let size = deepSizeOf(value.GetType(), pointerSize)        
        use buffer = new MemoryStream(size)
        use binWriter = new BinaryWriter(buffer)
        let newFixup = new List<Fixup>()
        
        // serialize object fields
        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public        
        value.GetType().GetFields(flags)
        |> Array.iter(fun field ->
            let fieldValue = field.GetValue(value)
            if fieldValue = null then                
                if field.FieldType.IsArray
                then getFieldArrayLength(field, pointerSize, new Dictionary<Type, Int32>())
                else deepSizeOf(field.FieldType, pointerSize)
                |> Array.zeroCreate<Byte>
                |> binWriter.Write

            elif fieldValue.GetType().IsArray then
                let arrayLength = field.GetCustomAttribute<MarshalAsAttribute>().SizeConst
                let arrayValue = (fieldValue :?> IEnumerable).GetEnumerator()
                
                for i=0 to arrayLength - 1 do
                    if arrayValue.MoveNext() then
                        serialize(arrayValue.Current, entries, true, fixups, analyzedObjects)
                        |> fun (buffer, _) -> binWriter.Write(buffer)
                                
            elif fieldValue.GetType().IsClass then                
                if analyzedObjects.Add(fieldValue) then
                    serialize(fieldValue, entries, true, fixups, analyzedObjects) |> ignore
                     
                {
                    Offset = binWriter.BaseStream.Position
                    ReferencedField = fieldValue
                    This = value
                }
                |> newFixup.Add

                // write address placeholder to be fixed later
                if pointerSize = 32
                then binWriter.Write(uint32 0)
                else binWriter.Write(uint64 0)
                
            else
                match fieldValue with
                | :? Byte as v -> binWriter.Write(v)
                | :? Int16 as v -> binWriter.Write(v)
                | :? UInt16 as v -> binWriter.Write(v)
                | :? Int32 as v -> binWriter.Write(v)
                | :? UInt32 as v -> binWriter.Write(v)
                | :? Int64 as v -> binWriter.Write(v)
                | :? UInt64 as v -> binWriter.Write(v)
                | v ->
                    // serialized struct are not added to the list
                    let (buffer, createdFixup) = serialize(v, entries, false, fixups, analyzedObjects)
                    
                    // modify fixup in order to reference my object
                    createdFixup
                    |> Seq.iter(fun fixup ->
                        fixup.This <- value
                        fixup.Offset <- binWriter.BaseStream.Position + fixup.Offset
                    )
                    binWriter.Write(buffer)                
        )        
        
        fixups.AddRange(newFixup)
        if addEntry then 
            let entry = { Buffer = buffer.ToArray(); Object = value}
            entries.Add(entry)            
            (entry.Buffer, newFixup)
        else
            (buffer.ToArray(), newFixup)        

    let allocateMemoryForEntries(entries: MemoryEntry seq, mainObject: Object, mainObjectAddress: UInt64, memory: MemoryManager) =        
        let entriesFixup = new Dictionary<Object, MemoryEntry * UInt64>()

        // allocate a big chunk of memory
        let totalSize = entries |> Seq.sumBy(fun entry -> entry.Buffer.Length)
        let mutable currentAddress = memory.AllocateMemory(mainObjectAddress, totalSize, Permission.Readable)

        // copy main object first
        let mainObjectEntry = entries |> Seq.find(fun entry -> Object.ReferenceEquals(entry.Object, mainObject))
        entriesFixup.[mainObjectEntry.Object] <- (mainObjectEntry, currentAddress)
        currentAddress <- currentAddress + uint64 mainObjectEntry.Buffer.Length

        // copy all entries sequentially
        entries
        |> Seq.filter(fun entry -> Object.ReferenceEquals(entry.Object, mainObject) |> not)
        |> Seq.iter(fun entry ->
            entriesFixup.[entry.Object] <- (entry, currentAddress)
            currentAddress <- currentAddress + uint64 entry.Buffer.Length
        )

        entriesFixup

    let writeEntriesToMemory(entries: (MemoryEntry * UInt64) seq) =
        entries
        |> Seq.iter(fun (entry, address) -> this.WriteMemory(address, entry.Buffer, false))

    let applyPatches(entriesFixup: Dictionary<Object, MemoryEntry * UInt64>, fixups: List<Fixup>) =
        let entries = entriesFixup.Values |> Seq.map(fun (e, _) -> e) |> Seq.toList
        
        fixups
        |> Seq.iter(fun fixup ->
            let fieldEntry = entries |> Seq.find(fun entry -> Object.ReferenceEquals(entry.Object, fixup.ReferencedField))
            let valueEntry = entries |> Seq.find(fun entry -> Object.ReferenceEquals(entry.Object, fixup.This))
            let (_, fieldAddress) = entriesFixup.[fieldEntry.Object]    

            // go to specified offset
            use memStream = new MemoryStream(valueEntry.Buffer)
            memStream.Position <- fixup.Offset
            use binWriter = new BinaryWriter(memStream)
            
            // write the address
            if pointerSize = 32
            then binWriter.Write(uint32 fieldAddress)
            else binWriter.Write(uint64 fieldAddress)
        )

    let readObject(binReader: BinaryReader, objectType: Type) =
        let size = deepSizeOf(objectType, pointerSize)
        if objectType.IsClass then                        
            let address =
                if pointerSize = 32
                then binReader.ReadUInt32() |> uint64
                else binReader.ReadUInt64()
            
            if address <> 0UL 
            then this.ReadMemory(address, size)
            else Array.zeroCreate<Byte>(size)
        else
            binReader.ReadBytes(size)

    let rec deserialize(buffer: Byte array, objectInstance: Object, analyzedObjects: Dictionary<UInt64, Object>) =        
        use binReader = new BinaryReader(new MemoryStream(buffer))

        // set field values
        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public        
        objectInstance.GetType().GetFields(flags)
        |> Array.iter(fun field ->
            if field.FieldType.IsArray then
                let arrayLength = field.GetCustomAttribute<MarshalAsAttribute>().SizeConst
                let elementType = field.FieldType.GetElementType()
                let arrayValue = Array.CreateInstance(elementType, arrayLength)
                
                for i=0 to arrayLength - 1 do                    
                    let elementBuffer = readObject(binReader, elementType)
                    let elementObject =           
                        if elementType.IsPrimitive || elementType.IsValueType 
                        then Activator.CreateInstance(elementType) |> box
                        else Activator.CreateInstance(elementType)

                    deserialize(elementBuffer, elementObject, analyzedObjects)

                    arrayValue.SetValue(
                        (
                            if elementType.IsPrimitive || elementType.IsValueType 
                            then unbox elementObject
                            else elementObject
                        ), i
                    )
                    
                field.SetValue(objectInstance, arrayValue)

            elif field.FieldType.IsClass then
                let address =
                    if pointerSize = 32
                    then binReader.ReadUInt32() |> uint64
                    else binReader.ReadUInt64()

                if analyzedObjects.ContainsKey(address) then
                    field.SetValue(objectInstance, analyzedObjects.[address])
                else
                    // set the field value                 
                    let fieldInstance =
                        if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                        then Activator.CreateInstance(field.FieldType) |> box
                        else Activator.CreateInstance(field.FieldType)

                    analyzedObjects.[address] <- fieldInstance

                    let size = deepSizeOf(field.FieldType, pointerSize)
                    let fieldBuffer = this.ReadMemory(address, size)
                    deserialize(fieldBuffer, fieldInstance, analyzedObjects)   
                    
                    let effectiveValue = 
                        if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                        then unbox fieldInstance
                        else fieldInstance
                    
                    field.SetValue(objectInstance, effectiveValue)
            else
                match field.GetValue(objectInstance) with
                | :? Byte -> binReader.ReadByte() :> Object
                | :? Int16 -> binReader.ReadInt16() :> Object
                | :? UInt16 -> binReader.ReadUInt16() :> Object
                | :? Int32 -> binReader.ReadInt32() :> Object
                | :? UInt32 -> binReader.ReadUInt32() :> Object
                | :? Int64 -> binReader.ReadInt64() :> Object
                | :? UInt64 -> binReader.ReadUInt64() :> Object
                | _ -> 
                    let elementBuffer = readObject(binReader, field.FieldType)
                    let fieldValue =
                        if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                        then Activator.CreateInstance(field.FieldType) |> box
                        else Activator.CreateInstance(field.FieldType)

                    deserialize(elementBuffer, fieldValue, analyzedObjects)

                    if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                    then unbox fieldValue
                    else fieldValue
                |> fun objectValue -> field.SetValue(objectInstance, objectValue)
        )      

    member this.MemoryAccess = _memoryAccessEvent.Publish  
    member this.MemoryAccessViolation = _memoryAccessViolation.Publish
    member val Stack = createStack() with get, set
    member val Heap = createHeap() with get, set

    member internal this.Clear() =
        _va.Clear()
    
    member this.ReadMemory(address: UInt64, size: Int32) =        
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Read address)  
        
        let memRegion = getMemoryRegion(address)
        if not <| memRegion.Permission.HasFlag(Permission.Readable) then
            _memoryAccessViolation.Trigger(MemoryAccessOperation.Read address)  
            Array.empty
        else
            let offset = address - memRegion.BaseAddress |> int32
            let realSize = min (memRegion.Content.Length-offset) size
            BinHandler.ReadBytes(memRegion.Handler, address, realSize)

    member this.ReadMemory(address: UInt64, objectType: Type) =
        // read raw bytes
        let size = deepSizeOf(objectType, pointerSize)
        let buffer = this.ReadMemory(address, size)

        let objectInstance = 
            if objectType.IsPrimitive || objectType.IsValueType 
            then Activator.CreateInstance(objectType) |> box
            else Activator.CreateInstance(objectType)

        // deserialize data
        deserialize(buffer, objectInstance, new Dictionary<UInt64, Object>())
        
        // unbox object if necessary
        if objectType.IsPrimitive || objectType.IsValueType 
        then unbox objectInstance
        else objectInstance

    member this.ReadMemory<'T>(address: UInt64) =
        this.ReadMemory(address, typeof<'T>) :?> 'T

    member this.GetMemoryRegion(address: UInt64) =
        getMemoryRegion(address)

    member this.WriteMemory(address: UInt64, value: Byte array, ?verifyProtection: Boolean) =
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Write (address, value))
        let region = this.GetMemoryRegion(address)

        match verifyProtection with
        | Some true when not <| region.Permission.HasFlag(Permission.Writable) ->
            _memoryAccessViolation.Trigger(MemoryAccessOperation.Write (address, value))
        | _ ->
            let offset = region.Handler.FileInfo.TranslateAddress address
            Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)
        
    member this.WriteMemory(address: UInt64, value: Object) =        
        let entries = new List<MemoryEntry>()
        let fixup = new List<Fixup>()
        serialize(value, entries, true, fixup, new HashSet<Object>()) |> ignore
        let fixedupEntries = allocateMemoryForEntries(entries, value, address, this)
        applyPatches(fixedupEntries, fixup)
        writeEntriesToMemory(fixedupEntries.Values)
        
    member this.UpdateMemoryRegion(baseAddress: UInt64, memoryRegion: MemoryRegion) =
        _va.[baseAddress] <- memoryRegion

    member this.IsAddressMapped(address: UInt64) =
        _va.Values
        |> Seq.exists(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address < endAddr
        )

    member this.AddMemoryRegion(memRegion: MemoryRegion) =
        _va.[memRegion.BaseAddress] <- memRegion

    member internal this.AddLibraryMemoryRegion(memRegion: MemoryRegion) =
        _lastAllocatedLibBase <- _lastAllocatedLibBase + uint64 memRegion.Content.Length
        let round = _lastAllocatedLibBase % 0x1000UL
        if round > 0UL then
            _lastAllocatedLibBase <- _lastAllocatedLibBase + 0x1000UL - round
        this.AddMemoryRegion(memRegion)

    member this.GetMemoryMap() =
        _va.Values 
        |> Seq.sortBy(fun m -> m.BaseAddress)
        |> Seq.readonly 
        |> Seq.toArray

    member this.FreeMemoryRegion(address: UInt64) =        
        let region = this.GetMemoryRegion(address)
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Free region)
        _va.Remove(region.BaseAddress) 
        
    member internal this.AllocateMemory(baseAddress: UInt64, size: Int32, permission: Permission) =
        let region = createMemoryRegion(baseAddress, size, permission)
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Allocate region)
        this.AddMemoryRegion(region)
        baseAddress

    member this.GetFreeMemory(size: Int32, ?startSearchFromAddress: UInt64) =
        let memoryMap = this.GetMemoryMap()
        if memoryMap.Length = 0 then
            // default initial allocation address
            0x400000UL
        elif memoryMap.Length = 1 then
            let lastRegion = memoryMap |> Array.last
            lastRegion.BaseAddress + uint64 lastRegion.Content.LongLength
        else
            memoryMap
            |> Seq.filter(fun m -> 
                match startSearchFromAddress with
                | Some addr -> m.BaseAddress > addr
                | None -> true
            )
            |> Seq.pairwise
            |> Seq.tryFind(fun (m1, m2) ->
                let availableSize = m2.BaseAddress - (m1.BaseAddress + uint64 m1.Content.LongLength)
                availableSize > uint64 size
            )
            |> function
                | Some (m1, _) -> 
                    m1.BaseAddress + uint64 m1.Content.LongLength
                | None -> 
                    let lastRegion = memoryMap |> Array.last
                    lastRegion.BaseAddress + uint64 lastRegion.Content.LongLength

    member internal this.GetNextLibraryAllocationBase(size: Int32, ?startSearchFromAddress: UInt64) = 
        let freeMemoryBase = 
            match startSearchFromAddress with
            | Some sa -> this.GetFreeMemory(size, sa)
            | None -> this.GetFreeMemory(size)
            
        max _lastAllocatedLibBase freeMemoryBase

    member this.AllocateMemory(size: Int32, permission: Permission) =
        this.AllocateMemory(this.GetFreeMemory(size), size, permission)

    member this.AllocateMemory(value: Byte array, permission: Permission) =        
        let baseAddress = this.AllocateMemory(value.Length, permission)
        this.WriteMemory(baseAddress, value)
        baseAddress

    member this.AllocateMemory(value: Object, permission: Permission) =
        let size = deepSizeOf(value.GetType(), pointerSize)
        let baseAddress = this.AllocateMemory(size, permission)
        this.WriteMemory(baseAddress, value)
        baseAddress

    member this.ReadAsciiString(address: UInt64) =
        let asciiString = new StringBuilder()
        let mutable offset = address
        let mutable c = this.ReadMemory(offset, 1).[0]
        while c <> 0x00uy do
            asciiString.Append(char c) |> ignore
            offset <- offset + 1UL
            c <- this.ReadMemory(offset, 1).[0]
        asciiString.ToString()