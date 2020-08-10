namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model
open B2R2
open ES.Sojobo.Windows

module Winsock =
    let WSAStartup(sandbox: ISandbox, wVersionRequired: UInt32, lpWSAData: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }