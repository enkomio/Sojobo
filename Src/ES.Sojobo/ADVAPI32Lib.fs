namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module ADVAPI32 =
    let openProcessToken(sandbox: ISandbox, processHandle: UInt32, desiredAccess: UInt32, tokenHandle: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }