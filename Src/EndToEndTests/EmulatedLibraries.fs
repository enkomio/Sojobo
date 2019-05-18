module EmulatedLibraries

open System
open ES.Sojobo
open ES.Sojobo.Model

module Kernel32 =
    let getLastError(sandbox: ISandbox) = {
        ReturnValue = Some <| createInt32(0x57).Value
        Convention = CallingConvention.Cdecl
    }

