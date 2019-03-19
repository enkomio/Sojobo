namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile

module Model =
    type EmulatedType =
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
        IsTemp = false
        Type = t
    }

    let createVariable(name: String, t: EmulatedType) = 
        createVariableWithValue(name, t, BitVector.zero 1<rt>)

    type MemoryRegion = {
        BaseAddress: UInt64
        Size: Int64
        Protection: SectionKind
        Handler: BinHandler
  
        // if it is reserver/commit/etc...
        Type: String
  
        // contains additional information, like the name of the file mapped
        Info: String
    }