namespace ES.Sojobo

open System
open System.Runtime.InteropServices

module Win32 =
    let teb32Address = 0x7ff70000u
    let peb32Address = 0x7ffdf000u

    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_teb
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
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 12)>]
        Reserved: Byte array

        // ...
    }