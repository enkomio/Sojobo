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