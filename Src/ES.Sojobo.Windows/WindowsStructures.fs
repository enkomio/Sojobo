namespace ES.Sojobo.Windows

open System
open System.Runtime.InteropServices

(*
    The following structures follow the definition provided by MS.
    As general rule, reserved fields are not valorized.
    Serialization info:
        - Class type (as record) are serialized as pointer to anothe memory region
        - Add "Struct" attribute if the class must be serialized as struct and not as a pointer
        - For array type always add the "MarshalAs" with "SizeConst" property in order to know how many items must be serialized
*)
[<AutoOpen>]
module WindowsStructures =
    
    // https://docs.microsoft.com/en-us/windows/desktop/api/subauth/ns-subauth-_unicode_string
    [<CLIMutable>]
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type UNICODE_STRING<'T> = {
        Length: UInt16
        MaximumLength: UInt16
        Buffer: 'T
    }

    [<CLIMutable>]
    [<Struct>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type ListEntry<'T> = {
        mutable Forward: 'T
        mutable Backward: 'T
    }
    
    // https://www.aldeid.com/wiki/LDR_DATA_TABLE_ENTRY 
    // https://docs.microsoft.com/en-us/windows/win32/api/winternl/ns-winternl-peb_ldr_data  
    [<CLIMutable>]
    [<ReferenceEquality>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type LDR_DATA_TABLE_ENTRY<'T> = {
        mutable InLoadOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        mutable InMemoryOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        mutable InInitializationOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        DllBase: 'T
        EntryPoint: 'T
        Reserved3: 'T
        FullDllName: UNICODE_STRING<'T>
        BaseDllName: UNICODE_STRING<'T>        
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 3)>]
        Reserved5: 'T array        
        Reserved6: 'T
        TimeDateStamp: UInt32
    }

    // https://cheesehack.tistory.com/99
    // https://www.aldeid.com/wiki/PEB_LDR_DATA
    // https://docs.microsoft.com/en-us/windows/win32/api/winternl/ns-winternl-peb_ldr_data
    // https://www.geoffchappell.com/studies/windows/win32/ntdll/structs/peb_ldr_data.htm
    [<CLIMutable>]
    [<ReferenceEquality>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB_LDR_DATA<'T> = {
        Length: UInt32
        Initialized: UInt32
        SsHandle: 'T
        mutable InLoadOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        mutable InMemoryOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        mutable InInitializationOrderLinks: ListEntry<LDR_DATA_TABLE_ENTRY<'T>>
        EntryInProgress: UInt32
        ShutdownInProgress: UInt32
        ShutdownThreadId: UInt32
    }

    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-peb
    // https://www.nirsoft.net/kernel_struct/vista/PEB.html
    // https://www.aldeid.com/wiki/PEB-Process-Environment-Block
    [<CLIMutable>]
    [<ReferenceEquality>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB<'T> = {
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved1: Byte array
        BeingDebugged: Byte
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)>]
        Reserved2: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved3: 'T array
        Ldr: PEB_LDR_DATA<'T>
        ProcessParameters: 'T
        SubSystemData: 'T
        ProcessHeap: 'T
        FastPebLock: 'T
        AtlThunkSListPtr: 'T
        Reserved5: 'T
        Reserved6: UInt32
        Reserved7: 'T
        Reserved8: UInt32
        AtlThunkSListPtr32: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 45)>]
        Reserved9: 'T array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 96)>]
        Reserved10: Byte array
        PostProcessInitRoutine: 'T
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 128)>]
        Reserved11: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1)>]
        Reserved12: 'T array
        SessionId: UInt32
    }
    
    // https://docs.microsoft.com/en-us/windows/win32/api/winternl/ns-winternl-teb
    // https://www.nirsoft.net/kernel_struct/vista/TEB.html
    // https://en.wikipedia.org/wiki/Win32_Thread_Information_Block
    [<CLIMutable>]
    [<ReferenceEquality>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type TEB<'T> = {
        // TIB
        ExceptionList: 'T
        StackBase: 'T
        StackLimit: 'T
        SubSystemTib: 'T
        Version: 'T
        ArbitraryUserPointer: 'T
        Self: 'T

        // TEB
        EnvironmentPointer: 'T
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        ClientId: 'T array
        ActiveRpcHandle: 'T
        ThreadLocalStoragePointer: 'T
        ProcessEnvironmentBlock: PEB<'T>
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 399)>]
        Reserved2: 'T array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 1952)>]
        Reserved3: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 64)>]
        TlsSlots: 'T array        
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 8)>]
        Reserved4: Byte array
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 26)>]
        Reserved5: 'T array
        ReservedForOle: 'T
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 4)>]
        Reserved6: 'T array
        TlsExpansionSlots: 'T
    }