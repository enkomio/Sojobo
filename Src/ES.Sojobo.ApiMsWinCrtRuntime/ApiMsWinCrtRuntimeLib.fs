namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module ``api-ms-win-crt-runtime-l1-1-0`` =
    let exit(sandbox: ISandbox, status: Int32) = 
        sandbox.Stop()
        {
            ReturnValue = None
            Convention = CallingConvention.Cdecl
        }

    let _initterm_e(sandbox: ISandbox, arg0: UInt32, arg1: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let _initterm(sandbox: ISandbox, arg0: UInt32, arg1: UInt32) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }
    
    let __p___argv(sandbox: ISandbox) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }

    let __p___argc(sandbox: ISandbox) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }

    let _get_initial_narrow_environment(sandbox: ISandbox) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }