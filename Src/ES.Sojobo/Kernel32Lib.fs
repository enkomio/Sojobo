namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module Kernel32 =
    let queryPerformanceCounter(win32Process: IProcessContainer) =
        {ReturnValue = None; Convention = CallingConvention.Cdecl}

    let getSystemTimeAsFileTime(win32Process: IProcessContainer, lpSystemTimeAsFileTime: UInt32) =
        {ReturnValue = None; Convention = CallingConvention.Cdecl}

    let getCurrentThreadId(win32Process: IProcessContainer) =
        let threadId = {createInt32(123) with Name = "EAX"; IsTemp=false}
        win32Process.SetVariable(threadId)
        Console.WriteLine("Return thread Id: 123")
        {ReturnValue = None; Convention = CallingConvention.Cdecl}
        
    let memset(win32Process: IProcessContainer) =
        let dest = win32Process.GetArgument(0)
        let c = win32Process.GetArgument(1)
        let count = win32Process.GetArgument(2)

        Console.WriteLine("memset")
        {ReturnValue = None; Convention = CallingConvention.Cdecl}
        
    let isProcessorFeaturePresent(win32Process: IProcessContainer) =
        Console.WriteLine("isProcessorFeaturePresent")
        {ReturnValue = None; Convention = CallingConvention.Cdecl}

    let getCurrentProcessId(win32Process: IProcessContainer) =
        let threadId = {createInt32(-1) with Name = "EAX"; IsTemp=false}
        win32Process.SetVariable(threadId)
        Console.WriteLine("Return current process Id: -1")
        {ReturnValue = None; Convention = CallingConvention.Cdecl}
