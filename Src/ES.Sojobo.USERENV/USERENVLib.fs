namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module USERENV =
    let loadUserProfileA(sandbox: ISandbox, hToken: UInt32, lpProfileInfo: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let loadUserProfileW(sandbox: ISandbox, hToken: UInt32, lpProfileInfo: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }
