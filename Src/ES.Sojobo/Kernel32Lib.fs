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
        let memoryManager = sandbox.GetRunningProcess().Memory        
        let mutable protection: MemoryProtection option = None
       
        // check execute
        if 
            flProtect &&& 0x10ul <> 0ul || 
            flProtect &&& 0x20ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x80ul <> 0ul
        then   
            protection <-
                match protection with
                | Some p -> p ||| MemoryProtection.Execute
                | None -> MemoryProtection.Execute
                |> Some

        // check write
        if 
            flProtect &&& 0x04ul <> 0ul || 
            flProtect &&& 0x08ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x80ul <> 0ul
        then   
            protection <-
                match protection with
                | Some p -> p ||| MemoryProtection.Write
                | None -> MemoryProtection.Write
                |> Some

        // check read
        if 
            flProtect &&& 0x20ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x02ul <> 0ul ||
            flProtect &&& 0x04ul <> 0ul
        then   
            protection <-
                match protection with
                | Some p -> p ||| MemoryProtection.Read
                | None -> MemoryProtection.Read
                |> Some
        
        let baseAddress = memoryManager.AllocateMemory(int32 dwSize, Option.defaultValue MemoryProtection.Read protection)

        {
            ReturnValue = Some <| createUInt32(uint32 baseAddress).Value
            Convention = CallingConvention.Cdecl
        }

    let virtualFree(sandbox: ISandbox, lpAddress: UInt32, dwSize: UInt32, dwFreeType: UInt32) = 
        let memoryManager = sandbox.GetRunningProcess().Memory      
        memoryManager.FreeMemoryRegion(uint64 lpAddress) |> ignore
        {
            ReturnValue = Some <| createInt32(0).Value
            Convention = CallingConvention.Cdecl
        }

    let getModuleHandleW(sandbox: ISandbox, lpModuleName: UInt32) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }

    let getLastError(sandbox: ISandbox) = {
        ReturnValue = Some <| createInt32(0).Value
        Convention = CallingConvention.Cdecl
    }