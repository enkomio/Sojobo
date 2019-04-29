namespace ES.Sojobo

open System
open System.Collections.Generic
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

    let createVariableWithValue(name: String, t: EmulatedType, v: BitVector) = {
        Name = name
        Value = v
        IsTemp = String.IsNullOrEmpty(name)
        Type = t
    }

    let createVariable(name: String, t: EmulatedType) = 
        createVariableWithValue(name, t, BitVector.zero 1<rt>)

    type MemoryRegion = {
        BaseAddress: UInt64
        Content: Byte array
        Protection: SectionKind
        Handler: BinHandler
        Type: String
        Info: String
    }

    let createMemoryRegion(baseAddr: UInt64, size: Int32, protection: SectionKind) = 
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