namespace ES.Sojobo

open System
open System.Runtime.InteropServices

module Win32 =
    let teb32Address = 0x7ff70000u
    let peb32Address = 0x7ffdf000u

    // https://www.aldeid.com/wiki/LIST_ENTRY
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type LIST_ENTRY = {
        Flink: UInt32
        Blink: UInt32
    }

    // https://docs.microsoft.com/en-us/windows/desktop/api/subauth/ns-subauth-_unicode_string
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type UNICODE_STRING = {
        Length: UInt16
        MaximumLength: UInt16
        Buffer: UInt32
    }
    
    // https://www.aldeid.com/wiki/LDR_DATA_TABLE_ENTRY
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type LDR_DATA_TABLE_ENTRY = {
        InLoadOrderLinks: LIST_ENTRY
        InMemoryOrderLinks: LIST_ENTRY
        InInitializationOrderLinks: LIST_ENTRY
        DllBase: UInt32
        EntryPoint: UInt32
        SizeOfImage: UInt32
        FullDllName: UNICODE_STRING
        BaseDllName: UNICODE_STRING
        Flags: UInt32
        LoadCount: UInt16
        TlsIndex: UInt16
        HashLinks: LIST_ENTRY
        LoadedImports: UInt32
        EntryPointActivationContext: UInt32
        PatchInformation: UInt32
        ForwarderLinks: LIST_ENTRY
        ServiceTagLinks: LIST_ENTRY
        StaticLinks: LIST_ENTRY
    }

    // https://www.aldeid.com/wiki/PEB_LDR_DATA
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB_LDR_DATA = {
        Length: UInt32
        Initialized: UInt32
        SsHandle: UInt32
        InLoadOrderModuleList: UInt32
        InMemoryOrderModuleList: UInt32
        InInitializationOrderModuleList: UInt32
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
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB32 = {
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved1: Byte array
        BeingDebugged: Byte
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)>]
        Reserved2: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved3: Byte array
        Ldr: UInt32
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