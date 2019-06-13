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

type private Fixup = {
    Offset: Int64
    FieldHashCode: Int32
    SourceHashCode: Int32
}

type private MemoryEntry = {
    HashCode: Int32
    Buffer: Byte array
}

type MemoryManager(pointerSize: Int32) =
    let _va = new Dictionary<UInt64, MemoryRegion>() 
    let _memoryAccessEvent = new Event<MemoryAccessOperation>()

    let createStack() =
        let stack = 
            createMemoryRegion(
                0x18C000UL, 
                0x4000, 
                Permission.Readable ||| Permission.Writable
            )
        _va.Add(stack.BaseAddress, stack)
        stack

    let createHeap() =
        let heap = 
            createMemoryRegion(
                0x520000UL, 
                0x16000,
                Permission.Readable ||| Permission.Writable
            )
        _va.Add(heap.BaseAddress, heap)
        heap

    let getMemoryRegion(address: UInt64) =
        _va.Values
        |> Seq.find(fun memRegion -> 
            let startAddr = memRegion.BaseAddress
            let endAddr = memRegion.BaseAddress + uint64 memRegion.Content.Length
            address >= startAddr && address < endAddr
        )

    let readMemory(address: UInt64, size: Int32) =
        let memRegion = getMemoryRegion(address)
        if memRegion.Permission <> Permission.Readable then
            // TODO: add check on memory protection
            ()

        let offset = address - memRegion.BaseAddress |> int32
        let realSize = min (memRegion.Content.Length-offset) size
        BinHandler.ReadBytes(memRegion.Handler, address, realSize)

    let rec getFieldArrayLength(field: FieldInfo, computedSize: Dictionary<Type, Int32>) =
        let arrayLength = field.GetCustomAttribute<MarshalAsAttribute>().SizeConst
        let elementType = field.FieldType.GetElementType()
        arrayLength * calculateSize(elementType, computedSize)

    and calculateSize(objectType: Type, computedSize: Dictionary<Type, Int32>) =
        if computedSize.ContainsKey(objectType) then
            computedSize.[objectType]

        elif objectType.IsValueType then
            Marshal.SizeOf(objectType)

        elif objectType.IsArray then
            let arrayLength = objectType.GetCustomAttribute<MarshalAsAttribute>().SizeConst
            let elementType = objectType.GetElementType()
            arrayLength * calculateSize(elementType, computedSize)

        elif objectType.IsClass then
            let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public        
            objectType.GetFields(flags)
            |> Array.sumBy(fun field ->
                if field.FieldType.IsArray then
                    getFieldArrayLength(field, computedSize)
                elif field.FieldType.IsClass then
                    pointerSize / 8
                else
                    Marshal.SizeOf(field.FieldType)
            )
            |> fun totalSize ->
                computedSize.[objectType] <- totalSize
                totalSize
        else
            failwith("Unable to get size of type: " + objectType.FullName)

    let rec serializeImpl(value: Object, entries: List<MemoryEntry>, fixups: List<Fixup>, analyzedObjects: HashSet<Object>) : Byte array =
        // allocate buffer
        let size = calculateSize(value.GetType(), new Dictionary<Type, Int32>())        
        use buffer = new MemoryStream(size)
        use binWriter = new BinaryWriter(buffer)
        
        // serialize object fields
        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public        
        value.GetType().GetFields(flags)
        |> Array.iter(fun field ->
            let fieldValue = field.GetValue(value)
            if fieldValue = null then
                let size = getFieldArrayLength(field, new Dictionary<Type, Int32>())
                let nullValue = Array.zeroCreate<Byte>(size)
                binWriter.Write(nullValue)

            elif field.FieldType.IsArray then
                let arrayLength = field.GetCustomAttribute<MarshalAsAttribute>().SizeConst
                let arrayValue = (fieldValue :?> IEnumerable).GetEnumerator()
                
                for i=0 to arrayLength - 1 do
                    if arrayValue.MoveNext() then
                        binWriter.Write(serializeImpl(arrayValue.Current, entries, fixups, analyzedObjects))
                                
            elif field.FieldType.IsClass then                
                if analyzedObjects.Add(fieldValue) then
                    serializeImpl(fieldValue, entries, fixups, analyzedObjects) |> ignore
                     
                {
                    SourceHashCode = value.GetHashCode()
                    FieldHashCode = fieldValue.GetHashCode()
                    Offset = binWriter.BaseStream.Position
                }
                |> fixups.Add

                // write address placeholder to be fixed later
                if pointerSize = 32
                then binWriter.Write(uint32 0)
                else binWriter.Write(uint64 0)
                
            else
                match field.GetValue(value) with
                | :? Byte as v -> binWriter.Write(v)
                | :? Int16 as v -> binWriter.Write(v)
                | :? UInt16 as v -> binWriter.Write(v)
                | :? Int32 as v -> binWriter.Write(v)
                | :? UInt32 as v -> binWriter.Write(v)
                | :? Int64 as v -> binWriter.Write(v)
                | :? UInt64 as v -> binWriter.Write(v)
                | v -> binWriter.Write(serializeImpl(v, entries, fixups, analyzedObjects))
        )

        // add entry
        let entry = {
            HashCode = value.GetHashCode()
            Buffer = buffer.ToArray()
        }
        entries.Add(entry)        
        entry.Buffer

    let rec serialize(value: Object, entries: List<MemoryEntry>, fixup: List<Fixup>): Byte array =        
        serializeImpl(value, entries, fixup, new HashSet<Object>())

    let allocateMemoryForEntries(entries: MemoryEntry seq, mainHashCode: Int32, mainObjectAddress: UInt64, memory: MemoryManager) =
        let entriesFixup = new Dictionary<Int32, MemoryEntry * UInt64>()

        // allocate a big chunk of memory
        let totalSize = entries |> Seq.sumBy(fun entry -> entry.Buffer.Length)
        let mutable currentAddress = memory.AllocateMemory(totalSize, Permission.Readable)

        // copy all entries to the just allocated memory
        entries
        |> Seq.iter(fun entry ->
            let address = 
                if entry.HashCode = mainHashCode 
                then createMemoryRegion(mainObjectAddress, entry.Buffer.Length, Permission.Readable).BaseAddress
                else currentAddress
            entriesFixup.[entry.HashCode] <- (entry, address)
            currentAddress <- currentAddress + uint64 entry.Buffer.Length
        )

        entriesFixup

    let writeEntriesToMemory(entries: (MemoryEntry * UInt64) seq, memory: MemoryManager) =
        entries
        |> Seq.iter(fun (entry, address) ->
            memory.UnsafeWriteMemory(address, entry.Buffer, false)
        )

    let applyPatches(entriesFixup: Dictionary<Int32, MemoryEntry * UInt64>, fixups: List<Fixup>) =
        let entries = entriesFixup.Values |> Seq.map(fun (e, _) -> e)
        
        fixups
        |> Seq.iter(fun fixup ->
            // get source entry and effective address
            let fieldEntry = entries |> Seq.find(fun entry -> entry.HashCode = fixup.FieldHashCode)
            let (_, fieldAddress) = entriesFixup.[fieldEntry.HashCode]
            let valueEntry = entries |> Seq.find(fun entry -> entry.HashCode = fixup.SourceHashCode)

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
        let size = calculateSize(objectType, new Dictionary<Type, Int32>())
        if objectType.IsClass then                        
            let address =
                if pointerSize = 32
                then binReader.ReadUInt32() |> uint64
                else binReader.ReadUInt64()
            
            if address <> 0UL 
            then readMemory(address, size)
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
                    let fieldValue =
                        if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                        then Activator.CreateInstance(field.FieldType) |> box
                        else Activator.CreateInstance(field.FieldType)

                    analyzedObjects.[address] <- fieldValue

                    let size = calculateSize(field.FieldType, new Dictionary<Type, Int32>())
                    let fieldBuffer = readMemory(address, size)
                    deserialize(fieldBuffer, fieldValue, analyzedObjects)   
                    
                    field.SetValue(
                        (
                            if field.FieldType.IsPrimitive || field.FieldType.IsValueType 
                            then unbox fieldValue
                            else fieldValue
                        ), fieldValue
                    )
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
    member val Stack = createStack() with get, set
    member val Heap = createHeap() with get, set

    member internal this.Clear() =
        _va.Clear()
    
    member this.ReadMemory(address: UInt64, size: Int32) =        
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Read address)        
        readMemory(address, size)

    member this.ReadMemory<'T>(address: UInt64) =
        // read raw bytes
        let objectType = typeof<'T>
        let size = calculateSize(objectType, new Dictionary<Type, Int32>())
        let buffer = readMemory(address, size)

        let objectInstance = 
            if objectType.IsPrimitive || objectType.IsValueType 
            then Activator.CreateInstance<'T>() |> box
            else Activator.CreateInstance<'T>() :> Object

        // deserialize data
        deserialize(buffer, objectInstance, new Dictionary<UInt64, Object>())
        
        // unbox object if necessary
        if objectType.IsPrimitive || objectType.IsValueType 
        then unbox objectInstance
        else objectInstance :?> 'T

    member this.GetMemoryRegion(address: UInt64) =
        getMemoryRegion(address)

    member internal this.UnsafeWriteMemory(address: UInt64, value: Byte array, verifyProtection: Boolean) =        
        let region = this.GetMemoryRegion(address)
        if verifyProtection && region.Permission <> Permission.Writable then
            // TODO: add check on memory protection
            ()

        let offset = region.Handler.FileInfo.TranslateAddress address
        Array.Copy(value, 0, region.Handler.FileInfo.BinReader.Bytes, offset, value.Length)

    member this.WriteMemory(address: UInt64, value: Byte array) =
        _memoryAccessEvent.Trigger(MemoryAccessOperation.Write (address, value))
        this.UnsafeWriteMemory(address, value, true)
        
    member this.WriteMemory(address: UInt64, value: Object) =        
        let entries = new List<MemoryEntry>()
        let fixup = new List<Fixup>()
        serialize(value, entries, fixup) |> ignore
        let fixedupEntries = allocateMemoryForEntries(entries, value.GetHashCode(), address, this)
        applyPatches(fixedupEntries, fixup)
        writeEntriesToMemory(fixedupEntries.Values, this)
        
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

    member this.AllocateMemory(size: Int32, permission: Permission) =
        let memoryMap = this.GetMemoryMap()
        let baseAddress =
            if memoryMap.Length = 0 then
                // default initial allocation address
                0x400000UL
            elif memoryMap.Length = 1 then
                let lastRegion = memoryMap |> Array.last
                lastRegion.BaseAddress + uint64 lastRegion.Content.LongLength
            else
                this.GetMemoryMap()
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

        // create the memory region
        this.AllocateMemory(baseAddress, size, permission)

    member this.AllocateMemory(value: Byte array, permission: Permission) =        
        let baseAddress = this.AllocateMemory(value.Length, permission)
        this.WriteMemory(baseAddress, value)
        baseAddress

    member this.AllocateMemory(value: Object, permission: Permission) =
        let size = calculateSize(value.GetType(), new Dictionary<Type, Int32>())
        let baseAddress = this.AllocateMemory(size, permission)
        this.WriteMemory(baseAddress, value)
        baseAddress

    member this.ReadAsciiString(address: UInt64) =
        let memRegionContent = readMemory(address, Int32.MaxValue)
        let indexOfNullChar = Array.IndexOf(memRegionContent, 0uy)
        let stringBytes = Array.sub memRegionContent 0 indexOfNullChar
        Encoding.UTF8.GetString(stringBytes)