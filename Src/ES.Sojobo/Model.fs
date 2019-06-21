namespace ES.Sojobo

open System
open System.IO
open System.IO.Compression
open System.Text
open System.Numerics
open System.Collections.Generic
open Newtonsoft.Json
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

module Model =
    type BitVectorSerializer() =
        inherit JsonConverter<BitVector>()

        override this.WriteJson(writer: JsonWriter, value: BitVector, serializer: JsonSerializer) =
            let rawValue = BitVector.getValue value      
            let serializedValue = String.Format("{0}|{1}", int32 <| BitVector.getType value, rawValue)            
            writer.WriteValue(serializedValue)

        override this.ReadJson(reader: JsonReader, objectType: Type, existingValue: BitVector, hasExistingValue: Boolean, serializer: JsonSerializer) =
            let rawValue = reader.Value :?> String
            let items = rawValue.Split([|'|'|])
            let (size, value) = (Int32.Parse(items.[0]), BigInteger.Parse(items.[1]))            
            BitVector.ofUBInt value (LanguagePrimitives.Int32WithMeasure<rt> size)

    type RegisterSnapshot = {
        Name: String
        Value: BitVector
        Size: Int32
        IsTemp: Boolean
    }

    type SymbolDto = {
        Name: String
        Library: String
        Address: UInt64
    }

    type LibrarySnapshot = {
        Name: String
        EntryPoint: UInt64
        BaseAddress: UInt64
        Exports: List<SymbolDto>
    }

    type MemoryRegionSnapshot = {
        Id: Guid
        BaseAddress: UInt64
        Content: Byte array
        Permission: Int32
        Type: String
        Info: String
    }
        
    type Snapshot = {
        Date: DateTime
        HeapRegionId: Guid
        StackRegionId: Guid
        VirtualAddressSpace: MemoryRegionSnapshot array
        Registers: RegisterSnapshot array
        Libraries: LibrarySnapshot array
    } with
        member this.SaveTo(stream: Stream) =
            use archive = new ZipArchive(stream, ZipArchiveMode.Create)

            // add all memory regions
            this.VirtualAddressSpace
            |> Array.iter(fun region ->
                let entryName = String.Format("mem_{0}.vmem", region.BaseAddress.ToString("X"))
                let zipEntry = archive.CreateEntry(entryName)
                use stream = zipEntry.Open()
                stream.Write(region.Content, 0, region.Content.Length)
            )

            // remove memory content since it was saved in separetd files
            let snapshotToSave = 
                {this with 
                    VirtualAddressSpace = 
                        this.VirtualAddressSpace
                        |> Array.map(fun region -> 
                            {region with Content = Array.empty<Byte>}
                        )
                }

            // serialize snapshot
            let serializedSnapshot = JsonConvert.SerializeObject(snapshotToSave, Formatting.Indented, new BitVectorSerializer())
            let zipEntry = archive.CreateEntry("snapshot.json")
            use streamWriter = new StreamWriter(zipEntry.Open())
            streamWriter.Write(serializedSnapshot)

        member this.SaveTo(filename: String) =
            use memoryStream = new MemoryStream()
            this.SaveTo(memoryStream)            
            File.WriteAllBytes(filename, memoryStream.ToArray())

        static member Read(stream: Stream) =
            use archive = new ZipArchive(stream)

            // deserialize snapshot
            let snapshotEntry = archive.GetEntry("snapshot.json")
            use streamReader = new StreamReader(snapshotEntry.Open())
            let serializedSnapshot = JsonConvert.DeserializeObject<Snapshot>(streamReader.ReadToEnd(), new BitVectorSerializer())

            // read all memory chunks
            let addressSpace =
                serializedSnapshot.VirtualAddressSpace
                |> Array.map(fun region ->
                    {region with 
                        Content = 
                            let entryName = String.Format("mem_{0}.vmem", region.BaseAddress.ToString("X"))
                            let memEntry = archive.GetEntry(entryName)
                            use memStream = new MemoryStream()
                            memEntry.Open().CopyTo(memStream)
                            memStream.ToArray()
                    }                    
                )

            // return result
            {serializedSnapshot with VirtualAddressSpace = addressSpace}

        static member Read(filename: String) =
            let fileContent = File.ReadAllBytes(filename)
            use memoryStream = new MemoryStream(fileContent)
            Snapshot.Read(memoryStream)

    type EmulatedType =
        | Bit
        | Byte
        | Word
        | DoubleWord
        | QuadWord

    type EmulatedValue = {
        Name: String
        IsTemp: Boolean
        Value: BitVector
        Type: EmulatedType
    }

    let internal createVariableWithValue(name: String, t: EmulatedType, v: BitVector) = {
        Name = name
        Value = v
        IsTemp = String.IsNullOrEmpty(name)
        Type = t
    }
    
    type MemoryRegion = {
        BaseAddress: UInt64
        Content: Byte array
        Permission: Permission
        Handler: BinHandler
        Type: String
        Info: String
    }

    type MemoryAccessOperation =
        | Read of UInt64
        | Write of UInt64 * Byte array
        | Allocate of MemoryRegion
        | Free of MemoryRegion

    type CallingConvention =
        | Cdecl
        | Stdecl

    type CallbackResult = {
        ReturnValue: BitVector option
        Convention: CallingConvention
    }

    let internal createVariable(name: String, t: EmulatedType) = 
        createVariableWithValue(name, t, BitVector.zero 1<rt>)

    let createInt32(value: Int32) =
        createVariableWithValue(String.Empty, EmulatedType.DoubleWord, BitVector.ofInt32 value 32<rt>)

    let createUInt32(value: UInt32) =
        createVariableWithValue(String.Empty, EmulatedType.DoubleWord, BitVector.ofUInt32 value 32<rt>)

    let createUInt64(value: UInt64) =
        createVariableWithValue(String.Empty, EmulatedType.DoubleWord, BitVector.ofUInt64 value 64<rt>)
        
    let internal createMemoryRegion(baseAddr: UInt64, size: Int32, permission: Permission) = 
        let content = Array.zeroCreate<Byte>(size)
        let isa = ISA.OfString "x86"        
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, baseAddr, content)

        {
            BaseAddress = baseAddr
            Content = content
            Permission = permission
            Handler = handler
            Type = String.Empty
            Info = String.Empty
        }