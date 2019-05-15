namespace ES.Sojobo

open System
open System.Reflection.PortableExecutable
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

module Model =

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

    [<Flags>]
    type MemoryProtection =
        | Read = 2
        | Write = 4
        | Execute = 8

    type MemoryRegion = {
        BaseAddress: UInt64
        Content: Byte array
        Protection: MemoryProtection
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

    let internal createInt32(value: Int32) =
        createVariableWithValue(String.Empty, EmulatedType.DoubleWord, BitVector.ofInt32 value 32<rt>)

    let internal createUInt32(value: UInt32) =
        createVariableWithValue(String.Empty, EmulatedType.DoubleWord, BitVector.ofUInt32 value 32<rt>)
        
    let internal createMemoryRegion(baseAddr: UInt64, size: Int32, protection: MemoryProtection) = 
        let content = Array.zeroCreate<Byte>(size)
        let isa = ISA.OfString "x86"        
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, baseAddr, content)

        {
            BaseAddress = baseAddr
            Content = content
            Protection = protection
            Handler = handler
            Type = String.Empty
            Info = String.Empty
        }