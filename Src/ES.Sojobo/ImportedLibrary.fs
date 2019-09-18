namespace ES.Sojobo
(*
open System
open System.Collections.Generic

type Library =
    | Managed of assembly: ManagedLibrary
    | Native of library: NativeLibrary

[<AutoOpen>]
module LibraryUtility =
    let getNativeLibraries(libraries: List<Library>) =
        libraries
        |> Seq.filter(fun lib ->
            match lib with
            | Native _ -> true
            | _ -> false
        )
        |> Seq.map(fun lib ->
            match lib with
            | Native lib -> lib
            | _ -> failwith "Not reached"
        )
        |> Seq.toArray

    let getManagedLibraries(libraries: List<Library>) =
        libraries
        |> Seq.filter(fun lib ->
            match lib with
            | Managed _ -> true
            | _ -> false
        )
        |> Seq.map(fun lib ->
            match lib with
            | Managed lib -> lib
            | _ -> failwith "Not reached"
        )
        |> Seq.toArray
        *)