namespace ES.Sojobo

open System
open System.Reflection
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile.PE
open B2R2.BinFile
open System.Reflection.PortableExecutable

module internal Helpers = 

    let toArray(bitVector: BitVector) =
        let size = int32 <| BitVector.getType bitVector
        let value = BitVector.getValue bitVector
        match size with        
        | 8 -> [|byte value|]
        | 16 -> BitConverter.GetBytes(uint16 value)
        | 32 -> BitConverter.GetBytes(uint32 value)
        | 64 -> BitConverter.GetBytes(uint64 value)
        | _ -> failwith("Unexpected size: " + string size)
        
    let getType(regType: RegType) =
        match (RegType.toBitWidth regType) with
        | 1 -> EmulatedType.Bit
        | 8 -> EmulatedType.Byte
        | 16 -> EmulatedType.Word
        | 32 -> EmulatedType.DoubleWord
        | 64 -> EmulatedType.QuadWord
        | _ -> failwith("Invalid reg type size: " + regType.ToString())

    let getSize(emuType: EmulatedType) =
        match emuType with
        | EmulatedType.Bit -> 1
        | EmulatedType.Byte -> 8
        | EmulatedType.Word -> 16
        | EmulatedType.DoubleWord -> 32
        | EmulatedType.QuadWord -> 64

    let getTypeSize =
        getType >> getSize

    let getTempName(index: String, emuType: EmulatedType) =
        let size =  getSize(emuType)
        "T_" + string index + ":" + string size

    let getPe(handler: BinHandler) =
        let fileInfo = handler.FileInfo
        fileInfo.GetType().GetField("pe", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue(fileInfo) :?> PE

    let getSectionPermission(sectionHeader: SectionHeader) =
        let characteristics = sectionHeader.SectionCharacteristics
        let mutable permission: Permission option = None
        
        if characteristics.HasFlag(SectionCharacteristics.MemRead) then 
            permission <- Some Permission.Readable

        if characteristics.HasFlag(SectionCharacteristics.MemWrite) then 
            permission <-
                match permission with
                | Some p -> p ||| Permission.Writable
                | None -> Permission.Writable
                |> Some

        if characteristics.HasFlag(SectionCharacteristics.MemExecute) then 
            permission <-
                match permission with
                | Some p -> p ||| Permission.Executable
                | None -> Permission.Executable
                |> Some

        Option.defaultValue Permission.Readable permission

    let getFunctionKeyName(functioName: String, libraryName: String) =
        let keyName = (libraryName + "::" + functioName).ToLower()
        keyName.Replace(".dll", String.Empty)