namespace ES.Sojobo.Lib

open System
open System.IO
open B2R2.BinFile
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
        let mutable permission: Permission option = None
       
        // check execute
        if 
            flProtect &&& 0x10ul <> 0ul || 
            flProtect &&& 0x20ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x80ul <> 0ul
        then   
            permission <-
                match permission with
                | Some p -> p ||| Permission.Executable
                | None -> Permission.Executable
                |> Some

        // check write
        if 
            flProtect &&& 0x04ul <> 0ul || 
            flProtect &&& 0x08ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x80ul <> 0ul
        then   
            permission <-
                match permission with
                | Some p -> p ||| Permission.Writable
                | None -> Permission.Writable
                |> Some

        // check read
        if 
            flProtect &&& 0x20ul <> 0ul || 
            flProtect &&& 0x40ul <> 0ul ||
            flProtect &&& 0x02ul <> 0ul ||
            flProtect &&& 0x04ul <> 0ul
        then   
            permission <-
                match permission with
                | Some p -> p ||| Permission.Readable
                | None -> Permission.Readable
                |> Some
        
        let baseAddress = memoryManager.AllocateMemory(int32 dwSize, Option.defaultValue Permission.Readable permission)

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

    let loadLibraryA(sandbox: ISandbox, lpLibFileName: UInt32) = 
        let libPath =
            if sandbox.GetRunningProcess().GetPointerSize() = 32
            then Environment.GetFolderPath(Environment.SpecialFolder.SystemX86)
            else Environment.GetFolderPath(Environment.SpecialFolder.System)

        // load library
        let libName = sandbox.GetRunningProcess().Memory.ReadAsciiString(uint64 lpLibFileName)
        let filename = Path.Combine(libPath, libName)
        if File.Exists(filename) then
            sandbox.MapLibrary(filename)

        let libHandle =
            sandbox.GetRunningProcess().Handles 
            |> Array.tryFind(fun hdl ->
                match hdl with
                | Library info when info.Name.Equals(filename |> Path.GetFileName, StringComparison.OrdinalIgnoreCase) -> true
                | _ -> false
            )
            |> function
                | Some (Handle.Library {Name = _; Value = libHandle}) -> libHandle
                | _ -> 0UL

        {
            Convention = CallingConvention.Cdecl
            ReturnValue = 
                if sandbox.GetRunningProcess().GetPointerSize() = 32
                then createUInt32(uint32 libHandle).Value
                else createUInt64(libHandle).Value
                |> Some            
        }