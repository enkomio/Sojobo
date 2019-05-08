namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module Kernel32 =
    let queryPerformanceCounter(baseProcess: IProcessContainer, lpPerformanceCount: UInt32) = {
        ReturnValue = Some <| createInt32(1).Value
        Convention = CallingConvention.Cdecl
    }

    let getSystemTimeAsFileTime(baseProcess: IProcessContainer, lpSystemTimeAsFileTime: UInt32) = {
        ReturnValue = None
        Convention = CallingConvention.Cdecl
    }

    let getCurrentThreadId(baseProcess: IProcessContainer) = {
        ReturnValue = Some <| createInt32(123).Value
        Convention = CallingConvention.Cdecl
    }

    let getCurrentProcessId(baseProcess: IProcessContainer) = {
        ReturnValue = Some <| createInt32(-1).Value
        Convention = CallingConvention.Cdecl
    }
        
    let isProcessorFeaturePresent(baseProcess: IProcessContainer, processorFeature: UInt32) = {
        ReturnValue = Some <| createInt32(1).Value
        Convention = CallingConvention.Cdecl
    }