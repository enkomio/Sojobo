namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module Kernel32 =
    let queryPerformanceCounter(sandbox: ISandbox, lpPerformanceCount: UInt32) = {
        ReturnValue = Some <| createInt32(1).Value
        Convention = CallingConvention.Cdecl
    }

    let getSystemTimeAsFileTime(sandbox: ISandbox, lpSystemTimeAsFileTime: UInt32) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }

    let getCurrentThreadId(sandbox: ISandbox) = {
        ReturnValue = Some <| createInt32(123).Value
        Convention = CallingConvention.Cdecl
    }

    let getCurrentProcessId(sandbox: ISandbox) = {
        ReturnValue = Some <| createInt32(-1).Value
        Convention = CallingConvention.Cdecl
    }
        
    let isProcessorFeaturePresent(sandbox: ISandbox, processorFeature: UInt32) = {
        ReturnValue = Some <| createInt32(1).Value
        Convention = CallingConvention.Cdecl
    }

    let isDebuggerPresent(sandbox: ISandbox) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let setUnhandledExceptionFilter(sandbox: ISandbox, lpTopLevelExceptionFilter: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let unhandledExceptionFilter(sandbox: ISandbox, exceptionInfo: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let virtualAlloc(sandbox: ISandbox, lpAddress: UInt32, dwSize: UInt32, flAllocationType: UInt32, flProtect: UInt32) = 
        let runningProc = sandbox.GetRunningProcess()
        let allocatedAddress = 123
        
        {
            ReturnValue = Some <| createInt32(allocatedAddress).Value
            Convention = CallingConvention.Cdecl
        }