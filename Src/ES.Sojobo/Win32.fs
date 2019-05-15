namespace ES.Sojobo

open System
open System.Text
open System.Runtime.InteropServices
open Model

module Win32 =
    let teb32Address = 0x7ff70000u

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
    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_peb_ldr_data
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type LDR_DATA_TABLE_ENTRY = {
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved1: UInt32 array
        InMemoryOrderLinks: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        Reserved2: UInt32 array
        DllBase: UInt32
        EntryPoint: UInt32
        SizeOfImage: UInt32
        FullDllName: UInt32
        BaseDllName: UInt32
        Flags: UInt32
        LoadCount: UInt16
        TlsIndex: UInt16
        HashLinks: UInt32
        LoadedImports: UInt32
        EntryPointActivationContext: UInt32
        PatchInformation: UInt32
        ForwarderLinks: UInt32
        ServiceTagLinks: UInt32
        StaticLinks: UInt32
    }

    // https://www.aldeid.com/wiki/PEB_LDR_DATA
    // https://docs.microsoft.com/en-us/windows/desktop/api/winternl/ns-winternl-_peb_ldr_data
    [<CLIMutable>]
    [<StructLayout(LayoutKind.Sequential, Pack=1, CharSet=CharSet.Ansi)>]
    type PEB_LDR_DATA = {
        Length: UInt32
        Initialized: UInt32
        SsHandle: UInt32
        InLoadOrderModuleList: LDR_DATA_TABLE_ENTRY
        InMemoryOrderModuleList: LDR_DATA_TABLE_ENTRY
        InInitializationOrderModuleList: LDR_DATA_TABLE_ENTRY
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
    (*
    let createPeb(proc: IProcessContainer) =
        let peb = Activator.CreateInstance<PEB32>()
                
        proc.GetImportedFunctions()
        |> Seq.groupBy(fun s -> s.LibraryName)
        |> Seq.map(fun (libraryName, _) -> libraryName)
        |> Seq.iter(fun libraryName ->
            let nameBytes = Encoding.Unicode.GetBytes(libraryName)
            let unicodeStringAddr =
                {Activator.CreateInstance<UNICODE_STRING>() with 
                    Length = uint16 nameBytes.Length
                    MaximumLength = uint16 nameBytes.Length   
                    Buffer = proc.Memory.AllocateMemory(nameBytes, MemoryProtection.Read) |> uint32
                }
                |> fun o -> uint32 <| proc.Memory.AllocateMemory(o, MemoryProtection.Read)

            let ldrDataTableEntry = 
                {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY>() with
                    SizeOfImage = 10ul
                    DllBase = 5000ul
                    EntryPoint = 0x1000ul
                    FullDllName = unicodeStringAddr
                    BaseDllName = unicodeStringAddr
                }
            
            let dataTableEntryAddress = proc.Memory.AllocateMemory(ldrDataTableEntry, MemoryProtection.Read)
            ()
        )

        ()

    *)