namespace ES.Sojobo

open System
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices
open System.Reflection
open ES.Sojobo.Model

// The following structures follow the definition provided by MS.
// As general rule, reserved fields are not valorized
module Win32 =

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
        mutable InLoadOrderLinks: LIST_ENTRY
        mutable InMemoryOrderLinks: LIST_ENTRY
        mutable InInitializationOrderLinks: LIST_ENTRY
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
        EnvironmentPointer: UInt32
        [<MarshalAs(UnmanagedType.ByValArray, SizeConst = 2)>]
        ClientId: UInt32 array
        ActiveRpcHandle: UInt32
        ThreadLocalStoragePointer: UInt32
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
                
        // create the entries list
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

        // write the uncompleted entries to memory
        let flags = BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
        let dataTableEntryRegionAddress = 
            proc.Memory.AllocateMemory(
                dataEntries.Count * Marshal.SizeOf<LDR_DATA_TABLE_ENTRY>(), 
                MemoryProtection.Read
            )
            
        // connect the link among them
        dataEntries        
        |> Seq.iteri(fun index entry -> 
            // compute various addresses
            let fIndex = (index + 1) % dataEntries.Count
            let fAddress = 
                uint64 (fIndex * Marshal.SizeOf<LDR_DATA_TABLE_ENTRY>()) + 
                dataTableEntryRegionAddress

            let bIndex = if index = 0 then dataEntries.Count - 1 else (index - 1) % dataEntries.Count
            let bAddress = 
                uint64 (bIndex * Marshal.SizeOf<LDR_DATA_TABLE_ENTRY>()) + 
                dataTableEntryRegionAddress

            // fill list entry
            entry.GetType().GetFields(flags) 
            |> Seq.filter(fun f -> f.FieldType = typeof<LIST_ENTRY>)                
            |> Seq.iter(fun field ->
                let offset = Marshal.OffsetOf<LDR_DATA_TABLE_ENTRY>(field.Name).ToInt32() |> uint32
                let listEntry = {
                    Flink = uint32 fAddress + offset
                    Blink = uint32 bAddress + offset
                }
                field.SetValue(entry, listEntry)
            )

            // finally write the entry to memory
            let address = 
                uint64 (index * Marshal.SizeOf<LDR_DATA_TABLE_ENTRY>()) + 
                dataTableEntryRegionAddress
            proc.Memory.WriteMemory(address, entry)
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
        let teb =
            {Activator.CreateInstance<TEB32>() with
                StackBase = uint32 stack.BaseAddress + uint32 stack.Content.Length
                StackLimit = uint32 stack.BaseAddress
                Self = 0x7ff70000u
                ProcessEnvironmentBlock = uint32 peb32Address
            }

        let tebRegion = 
            {createMemoryRegion(uint64 teb.Self, Marshal.SizeOf<TEB32>(), MemoryProtection.Read) with 
                Type = "TEB32"
            }
        proc.Memory.AddMemoryRegion(tebRegion)
        proc.Memory.WriteMemory(uint64 teb.Self, teb)
        tebRegion.BaseAddress