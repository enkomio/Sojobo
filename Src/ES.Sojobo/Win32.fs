namespace ES.Sojobo

open System
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices
open Model

// The following structures follow the definition provided by MS.
// As general rule, reserved fields are not valorized
module Win32 =
    
    // https://docs.microsoft.com/en-us/windows/desktop/api/subauth/ns-subauth-_unicode_string
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type UNICODE_STRING = {
        Length: UInt16
        MaximumLength: UInt16
        Buffer: UInt32
    }
    
    // https://www.aldeid.com/wiki/LDR_DATA_TABLE_ENTRY 
    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_peb_ldr_data
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type LDR_DATA_TABLE_ENTRY = {
        mutable InLoadOrderLinksFlink: LDR_DATA_TABLE_ENTRY
        mutable InLoadOrderLinksBlink: LDR_DATA_TABLE_ENTRY
        mutable InMemoryOrderLinksFlink: LDR_DATA_TABLE_ENTRY
        mutable InMemoryOrderLinksBlink: LDR_DATA_TABLE_ENTRY
        mutable InInitializationOrderLinksFlink: LDR_DATA_TABLE_ENTRY
        mutable InInitializationOrderLinksBlink: LDR_DATA_TABLE_ENTRY
        DllBase: UInt32
        EntryPoint: UInt32
        Reserved3: UInt32
        FullDllName: UNICODE_STRING
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)>]
        Reserved4: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>]
        Reserved5: UInt32 array        
        Reserved6: UInt32
        TimeDateStamp: UInt32
    }

    // https://www.aldeid.com/wiki/PEB_LDR_DATA
    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_peb_ldr_data
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB_LDR_DATA = {
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)>]
        Reserved1: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>]
        Reserved2: UInt32 array
        InMemoryOrderModuleList: LDR_DATA_TABLE_ENTRY
    }

    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_teb
    // https://www.nirsoft.net/kernel_struct/vista/TEB.html
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type TEB32 = {
        // TIB
        ExceptionList: UInt32
        StackBase: UInt32
        StackLimit: UInt32
        SubSystemTib: UInt32
        Version: UInt32
        ArbitraryUserPointer: UInt32
        Self: UInt32

        // TEB
        ProcessEnvironmentBlock: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 399)>]
        Reserved2: UInt32 array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1952)>]
        Reserved3: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 64)>]
        TlsSlots: UInt32 array        
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)>]
        Reserved4: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 26)>]
        Reserved5: UInt32 array
        ReservedForOle: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)>]
        Reserved6: UInt32 array
        TlsExpansionSlots: UInt32
    }

    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-peb
    // https://www.aldeid.com/wiki/PEB-Process-Environment-Block
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB32 = {
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved1: Byte array
        BeingDebugged: Byte
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)>]
        Reserved2: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved3: UInt32 array
        Ldr: PEB_LDR_DATA
        ProcessParameters: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>]
        Reserved4: Byte array
        AtlThunkSListPtr: UInt32
        Reserved5: UInt32
        Reserved6: UInt32
        Reserved7: UInt32
        Reserved8: UInt32
        AtlThunkSListPtr32: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 45)>]
        Reserved9: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 96)>]
        Reserved10: Byte array
        PostProcessInitRoutine: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)>]
        Reserved11: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)>]
        Reserved12: Byte array
        SessionId: UInt32
    }    
    
    let private createPeb(sandbox: ISandbox) =
        let proc = sandbox.GetRunningProcess()
        let dataEntries = new List<LDR_DATA_TABLE_ENTRY>()
                
        // fill the entries list
        proc.GetImportedFunctions()            
        |> Seq.groupBy(fun s -> s.LibraryName)
        |> Seq.map(fun (libraryName, _) -> libraryName)
        |> Seq.iter(fun libraryName ->
            let fullNameBytes = Encoding.Unicode.GetBytes(libraryName)
            let fullNameDll = 
                {Activator.CreateInstance<UNICODE_STRING>() with 
                    Length = uint16 fullNameBytes.Length
                    MaximumLength = uint16 fullNameBytes.Length   
                    Buffer = proc.Memory.AllocateMemory(fullNameBytes, MemoryProtection.Read) |> uint32
                }

            {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY>() with
                FullDllName = fullNameDll
            } 
            |> dataEntries.Add
        )

        // connect the FLink and BLink
        dataEntries        
        |> Seq.iteri(fun index entry ->
            let flink = dataEntries.[(index + 1) % dataEntries.Count]
            let blink = 
                if index = 0 
                then dataEntries.[dataEntries.Count - 1]
                else dataEntries.[(index - 1) % dataEntries.Count]

            dataEntries.[index].InLoadOrderLinksFlink <- flink
            dataEntries.[index].InLoadOrderLinksBlink <- blink
            dataEntries.[index].InMemoryOrderLinksFlink <- flink
            dataEntries.[index].InMemoryOrderLinksBlink <- blink
            dataEntries.[index].InInitializationOrderLinksFlink <- flink
            dataEntries.[index].InInitializationOrderLinksBlink <- blink
        )
        
        // finally create the PEB
        {Activator.CreateInstance<PEB32>() with 
            Ldr = 
                {Activator.CreateInstance<PEB_LDR_DATA>() with
                    InMemoryOrderModuleList = Seq.head dataEntries
                }
        }

    let createTeb(sandbox: ISandbox) =
        let proc = sandbox.GetRunningProcess()
        let peb = createPeb(sandbox)
        let peb32Address = proc.Memory.AllocateMemory(peb, MemoryProtection.Read)

        let stack = 
            proc.Memory.GetMemoryMap()
            |> Seq.find(fun memRegion -> memRegion.Type.Equals("Stack", StringComparison.OrdinalIgnoreCase))
        
        // create the TEB
        {Activator.CreateInstance<TEB32>() with
            StackBase = uint32 stack.BaseAddress + uint32 stack.Content.Length
            StackLimit = uint32 stack.BaseAddress
            Self = 0x7ff70000u
            ProcessEnvironmentBlock = uint32 peb32Address
        }
        |> fun teb -> proc.Memory.AllocateMemory(teb, MemoryProtection.Read)