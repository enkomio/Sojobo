namespace ES.Sojobo

open System
open System.IO

type NativeLibrary(content: Byte array) =
    member val Filename: String option = None with get, set

    static member Create(content: Byte array) =
        new NativeLibrary(content)

    static member Create(filename: String) =
        let content = File.ReadAllBytes(filename)
        new NativeLibrary(content, Filename = Some filename)