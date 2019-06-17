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

    type LibrarySnapshot = {
        Name: String
        EntryPoint: UInt64
        BaseAddress: UInt64
        Exports: List<Symbol>
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
            let serializedValue = JsonConvert.SerializeObject(this, Formatting.Indented, new BitVectorSerializer())

            // compress the value
            use inputtStream = new MemoryStream(Encoding.UTF8.GetBytes(serializedValue))
            use gs = new GZipStream(stream, CompressionMode.Compress)
            inputtStream.CopyTo(gs)

        member this.SaveTo(filename: String) =
            use memoryStream = new MemoryStream()
            this.SaveTo(memoryStream)            
            File.WriteAllBytes(filename, memoryStream.ToArray())

        static member Read(stream: Stream) =
            // decompress string
            use outputStream = new MemoryStream()
            use gs = new GZipStream(stream, CompressionMode.Decompress)
            gs.CopyTo(outputStream)
            let serializedString = Encoding.UTF8.GetString(outputStream.ToArray())

            // deserialize
            JsonConvert.DeserializeObject<Snapshot>(serializedString, new BitVectorSerializer())

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