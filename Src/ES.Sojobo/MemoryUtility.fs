namespace ES.Sojobo

open System
open System.Text

[<AutoOpen>]
module MemoryUtility =
    let readAsciiString(memManager: MemoryManager, address: UInt64) =
        let asciiString = new StringBuilder()
        let mutable offset = address
        let mutable c = memManager.ReadMemory(offset, 1).[0]
        while c <> 0x00uy do
            asciiString.Append(char c) |> ignore
            offset <- offset + 1UL
            c <- memManager.ReadMemory(offset, 1).[0]
        asciiString.ToString()

    let readUnicodeString(memManager: MemoryManager, address: UInt64) =
        let unicodeString = new StringBuilder()
        let mutable offset = address
        let mutable c = memManager.ReadMemory<UInt16>(offset)
        while c <> uint16 0 do
            if c > uint16 255 then
                // error
                c <- uint16 0
                unicodeString.Clear() |> ignore
            else
                unicodeString.Append(char c) |> ignore
                offset <- offset + 2UL
                c <- memManager.ReadMemory<UInt16>(offset)
        unicodeString.ToString()

