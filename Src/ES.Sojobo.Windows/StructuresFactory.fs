namespace ES.Sojobo.Windows

open System
open System.Collections.Generic
open System.Text
open System.IO
open ES.Sojobo
open B2R2.BinFile

module StructuresFactory =
    let buildPeb<'T when 'T: struct>(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()
        let librariesDetails = 
            sandbox.NativeLibraries
            |> Seq.map(fun lib -> (lib.GetLibraryName().ToLowerInvariant(), lib))
            |> dict

        let loadedLibraries =
            sandbox.NativeLibraries
            |> Seq.filter(fun lib -> lib.IsLoaded())
            |> Seq.toArray

        let createUnicodeString(libName: String, currentOffset: UInt64) =
            let fullNameBytes = 
                if String.IsNullOrEmpty(libName) then Array.zeroCreate<Byte>(proc.PointerSize / 8)
                else Encoding.Unicode.GetBytes(libName)
            proc.Memory.WriteMemory(currentOffset, fullNameBytes)       

            let unicodeString = 
                {Activator.CreateInstance<UNICODE_STRING<'T>>() with 
                    Length = uint16 fullNameBytes.Length
                    MaximumLength = uint16 fullNameBytes.Length   
                    Buffer = currentOffset :> Object :?> 'T
                }
            (currentOffset + uint64 fullNameBytes.Length, unicodeString)

        // allocate the memory for the unicode names
        let programSize = 
            let n = proc.FileName               
            if String.IsNullOrEmpty(n) then proc.PointerSize / 8 * 2
            else Encoding.Unicode.GetBytes(n).Length + Encoding.Unicode.GetBytes(Path.GetFileName(n)).Length                

        let modulesSize =
            loadedLibraries
            |> Array.sumBy(fun lib -> 
                Encoding.Unicode.GetBytes(lib.GetLibraryName()).Length +
                Encoding.Unicode.GetBytes(lib.GetFullName()).Length
            )

        let regionSize = programSize + modulesSize
        let mutable libraryNamesRegionOffset = proc.Memory.AllocateMemory(regionSize, Permission.Readable)
                        
        // create the data table entries
        let dataEntries = new Dictionary<Int32, LDR_DATA_TABLE_ENTRY<'T>>()

        // add as first module the current program
        let fullProgramName = sandbox.GetRunningProcess().FileName
        let (nextOffset, fullName) = createUnicodeString(fullProgramName, libraryNamesRegionOffset)
        let (_, baseName) = createUnicodeString(Path.GetFileName(fullProgramName), nextOffset)
        
        {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<'T>>() with
            FullDllName = fullName
            BaseDllName = baseName
            DllBase =
                let v = proc.GetActiveMemoryRegion().BaseAddress
                if proc.PointerSize = 32 then (uint32 v) :> Object :?> 'T
                else v :> Object :?> 'T
            EntryPoint = 
                let v = proc.GetActiveMemoryRegion().Handler.FileInfo.EntryPoint.Value
                if proc.PointerSize = 32 then (uint32 v) :> Object :?> 'T
                else v :> Object :?> 'T
        }
        |> fun entry -> dataEntries.Add(Int32.MaxValue, entry)

        // add all the loaded libraries by considering the priority of ntdll, kernel32 and kernelbase
        loadedLibraries
        |> Array.iter(fun lib ->
            // get details to insert into PEB
            let (imageBase, entryPoint) =
                match librariesDetails.TryGetValue(lib.GetLibraryName().ToLowerInvariant()) with
                | (true, lib) -> (lib.BaseAddress, lib.EntryPoint)
                | (false, _) -> (0UL, 0UL)
                            
            let position =
                // sort this list in order to have ntdll.dll and kernel32.dll on top
                if lib.GetLibraryName().Equals("ntdll.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 1
                elif lib.GetLibraryName().Equals("kernel32.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 2
                elif lib.GetLibraryName().Equals("kernelbase.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 3
                else lib.GetLibraryName().GetHashCode()

            // create fullname UnicodeString
            let (nextOffset, fullNameDll) = createUnicodeString(lib.GetFullName(), libraryNamesRegionOffset)
            let (nextOffset, baseNameDll) = createUnicodeString(lib.GetLibraryName(), nextOffset)
            libraryNamesRegionOffset <- nextOffset

            // create Data Table Entry
            {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<'T>>() with
                FullDllName = fullNameDll
                BaseDllName = baseNameDll
                DllBase = 
                    let v = imageBase
                    if proc.PointerSize = 32 then (uint32 v) :> Object :?> 'T
                    else v :> Object :?> 'T
                EntryPoint = 
                    let v = entryPoint                
                    if proc.PointerSize = 32 then (uint32 v) :> Object :?> 'T
                    else v :> Object :?> 'T
            }
            |> fun entry -> dataEntries.Add(position, entry)
        ) 

        // as last entry add an empty record
        dataEntries.Add(Int32.MinValue, Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<'T>>())
        
        // connect the link among them
        let sortedEntries =
            dataEntries
            |> Seq.sortByDescending(fun kv -> kv.Key)
            |> Seq.map(fun kv -> kv.Value)
            |> Seq.toArray

        let head = Seq.head sortedEntries
        let last = Seq.last sortedEntries

        sortedEntries        
        |> Seq.iteri(fun index entry ->
            let fEntry = 
                (index + 1) % sortedEntries.Length
                |> Array.get sortedEntries
            
            let bEntry = 
                if index = 0 then sortedEntries.Length - 1 
                else (index - 1) % sortedEntries.Length
                |> Array.get sortedEntries
           
            // set connection            
            entry.InInitializationOrderLinks.Forward <- fEntry
            entry.InInitializationOrderLinks.Backward <- bEntry
            entry.InMemoryOrderLinks.Forward <- fEntry
            entry.InMemoryOrderLinks.Backward <- bEntry
            entry.InLoadOrderLinks.Forward <- fEntry
            entry.InLoadOrderLinks.Backward <- bEntry                    
        )

        // connect Ldr to head and last
        let ldr =
            {Activator.CreateInstance<PEB_LDR_DATA<'T>>() with
                InInitializationOrderLinks = {Forward = head; Backward = last}
                InMemoryOrderLinks = {Forward = head; Backward = last}
                InLoadOrderLinks = {Forward = head; Backward = last}
                Initialized = 1u
                Length = uint32 <| Helpers.deepSizeOf(typeof<PEB_LDR_DATA<'T>>, proc.PointerSize)
            }
                                
        // finally create the PEB
        let zero =
            if proc.PointerSize = 32 then 0u :> Object :?> 'T
            else 0UL :> Object :?> 'T

        {Activator.CreateInstance<PEB<'T>>() with 
            Reserved1 = Array.zeroCreate<Byte>(2)
            BeingDebugged = 0uy
            Reserved2 = Array.zeroCreate<Byte>(1)
            Reserved3 = Array.zeroCreate<'T>(2)            
            Ldr = ldr
            ProcessParameters = zero
            SubSystemData = zero
            ProcessHeap =
                let v = proc.GetActiveMemoryRegion().BaseAddress
                if proc.PointerSize = 32 then (uint32 v) :> Object :?> 'T
                else v :> Object :?> 'T
            FastPebLock = zero
            AtlThunkSListPtr = zero
            Reserved5 = zero
            Reserved6 = 0u
            Reserved7 = zero
            Reserved8 = 0u
            AtlThunkSListPtr32 = 0u
            Reserved9 = Array.zeroCreate<'T>(45)
            Reserved10 = Array.zeroCreate<Byte>(96)
            PostProcessInitRoutine = zero
            Reserved11 = Array.zeroCreate<Byte>(128)
            Reserved12 = Array.zeroCreate<'T>(1)
            SessionId = 0u
        }
       
    let buildPeb32(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()
        let librariesDetails = 
            sandbox.NativeLibraries
            |> Seq.map(fun lib -> (lib.GetLibraryName().ToLowerInvariant(), lib))
            |> dict

        let loadedLibraries =
            sandbox.NativeLibraries
            |> Seq.filter(fun lib -> lib.IsLoaded())
            |> Seq.toArray

        let createUnicodeString(libName: String, currentOffset: UInt64) =
            let fullNameBytes = 
                if String.IsNullOrEmpty(libName) then Array.zeroCreate<Byte>(proc.PointerSize / 8)
                else Encoding.Unicode.GetBytes(libName)
            proc.Memory.WriteMemory(currentOffset, fullNameBytes)       

            let unicodeString = 
                {Activator.CreateInstance<UNICODE_STRING<UInt32>>() with 
                    Length = uint16 fullNameBytes.Length
                    MaximumLength = uint16 fullNameBytes.Length   
                    Buffer = uint32 currentOffset
                }
            (currentOffset + uint64 fullNameBytes.Length, unicodeString)

        // allocate the memory for the unicode names
        let programSize = 
            let n = proc.FileName               
            if String.IsNullOrEmpty(n) then proc.PointerSize / 8 * 2
            else Encoding.Unicode.GetBytes(n).Length + Encoding.Unicode.GetBytes(Path.GetFileName(n)).Length                

        let modulesSize =
            loadedLibraries
            |> Array.sumBy(fun lib -> 
                Encoding.Unicode.GetBytes(lib.GetLibraryName()).Length +
                Encoding.Unicode.GetBytes(lib.GetFullName()).Length
            )

        let regionSize = programSize + modulesSize
        let mutable libraryNamesRegionOffset = proc.Memory.AllocateMemory(regionSize, Permission.Readable)
                        
        // create the data table entries
        let dataEntries = new Dictionary<Int32, LDR_DATA_TABLE_ENTRY<UInt32>>()

        // add as first module the current program        
        let (nextOffset, fullName) = createUnicodeString(proc.FileName , libraryNamesRegionOffset)
        let (_, baseName) = createUnicodeString(Path.GetFileName(proc.FileName ), nextOffset)
        
        {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<UInt32>>() with
            FullDllName = fullName
            BaseDllName = baseName
            DllBase = uint32 <| proc.GetActiveMemoryRegion().BaseAddress
            EntryPoint = uint32 <| proc.GetActiveMemoryRegion().Handler.FileInfo.EntryPoint.Value
        }
        |> fun entry -> dataEntries.Add(Int32.MaxValue, entry)

        // add all the loaded libraries by considering the priority of ntdll, kernel32 and kernelbase
        loadedLibraries
        |> Array.iter(fun lib ->
            // get details to insert into PEB
            let (imageBase, entryPoint) =
                match librariesDetails.TryGetValue(lib.GetLibraryName().ToLowerInvariant()) with
                | (true, lib) -> (uint32 lib.BaseAddress, uint32 lib.EntryPoint)
                | (false, _) -> (0u, 0u)
                            
            let position =
                // sort this list in order to have ntdll.dll and kernel32.dll on top
                if lib.GetLibraryName().Equals("ntdll.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 1
                elif lib.GetLibraryName().Equals("kernel32.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 2
                elif lib.GetLibraryName().Equals("kernelbase.dll", StringComparison.OrdinalIgnoreCase) then Int32.MaxValue - 3
                else lib.GetLibraryName().GetHashCode()

            // create fullname UnicodeString
            let (nextOffset, fullNameDll) = createUnicodeString(lib.GetFullName(), libraryNamesRegionOffset)
            let (nextOffset, baseNameDll) = createUnicodeString(lib.GetLibraryName(), nextOffset)
            libraryNamesRegionOffset <- nextOffset

            // create Data Table Entry
            {Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<UInt32>>() with
                FullDllName = fullNameDll
                BaseDllName = baseNameDll
                DllBase = imageBase
                EntryPoint = entryPoint
            }
            |> fun entry -> dataEntries.Add(position, entry)
        ) 

        // as last entry add an empty record
        dataEntries.Add(Int32.MinValue, Activator.CreateInstance<LDR_DATA_TABLE_ENTRY<UInt32>>())
        
        // connect the link among them
        let sortedEntries =
            dataEntries
            |> Seq.sortByDescending(fun kv -> kv.Key)
            |> Seq.map(fun kv -> kv.Value)
            |> Seq.toArray

        let head = Seq.head sortedEntries
        let last = Seq.last sortedEntries

        sortedEntries        
        |> Seq.iteri(fun index entry ->
            let fEntry = 
                (index + 1) % sortedEntries.Length
                |> Array.get sortedEntries
            
            let bEntry = 
                if index = 0 then sortedEntries.Length - 1 
                else (index - 1) % sortedEntries.Length
                |> Array.get sortedEntries
           
            // set connection            
            entry.InInitializationOrderLinks.Forward <- fEntry
            entry.InInitializationOrderLinks.Backward <- bEntry
            entry.InMemoryOrderLinks.Forward <- fEntry
            entry.InMemoryOrderLinks.Backward <- bEntry
            entry.InLoadOrderLinks.Forward <- fEntry
            entry.InLoadOrderLinks.Backward <- bEntry                    
        )

        // connect Ldr to head and last
        let ldr =
            {Activator.CreateInstance<PEB_LDR_DATA<UInt32>>() with
                InInitializationOrderLinks = {Forward = head; Backward = last}
                InMemoryOrderLinks = {Forward = head; Backward = last}
                InLoadOrderLinks = {Forward = head; Backward = last}
                Initialized = 1u
                Length = uint32 <| Helpers.deepSizeOf(typeof<PEB_LDR_DATA<UInt32>>, proc.PointerSize)
            }
                                
        // finally create the PEB
        {Activator.CreateInstance<PEB<UInt32>>() with 
            Reserved1 = Array.zeroCreate<Byte>(2)
            BeingDebugged = 0uy
            Reserved2 = Array.zeroCreate<Byte>(1)
            Reserved3 = Array.zeroCreate<UInt32>(2)            
            Ldr = ldr
            ProcessParameters = 0u
            SubSystemData = 0u
            ProcessHeap = uint32 proc.Memory.Heap.BaseAddress
            FastPebLock = 0u
            AtlThunkSListPtr = 0u
            Reserved5 = 0u
            Reserved6 = 0u
            Reserved7 = 0u
            Reserved8 = 0u
            AtlThunkSListPtr32 = 0u
            Reserved9 = Array.zeroCreate<UInt32>(45)
            Reserved10 = Array.zeroCreate<Byte>(96)
            PostProcessInitRoutine = 0u
            Reserved11 = Array.zeroCreate<Byte>(128)
            Reserved12 = Array.zeroCreate<UInt32>(1)
            SessionId = 0u
        }

    let buildTeb32(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()        
        {Activator.CreateInstance<TEB<UInt32>>() with
            StackBase = uint32 proc.Memory.Stack.BaseAddress + uint32 proc.Memory.Stack.Content.Length
            StackLimit = uint32 proc.Memory.Stack.BaseAddress
            Self = 0x7ff70000u
            ProcessEnvironmentBlock = buildPeb32(sandbox)
        }

    let buildTeb64(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()        
        {Activator.CreateInstance<TEB<UInt64>>() with
            StackBase = proc.Memory.Stack.BaseAddress + uint64 proc.Memory.Stack.Content.Length
            StackLimit = proc.Memory.Stack.BaseAddress
            Self = 0x7ff70000UL
            ProcessEnvironmentBlock = buildPeb<UInt64>(sandbox)
        }

    let createTeb32(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()
        
        // create the TEB and fix address if necessary. 
        // I multiple by 2 just to reserve enough space and avoid to move it in the future
        let teb = buildTeb32(sandbox)
        let tebSize = Helpers.deepSizeOf(teb.GetType(), proc.PointerSize) * 2
        let tebAddress = proc.Memory.GetFreeMemory(tebSize, 0x7ff70000UL) 
        let teb = {teb with Self = uint32 tebAddress}

        // write TEB to memory
        proc.Memory.WriteMemory(uint64 teb.Self, teb)
        let tebRegion = {proc.Memory.GetMemoryRegion(uint64 teb.Self) with Info = "TEB"}
        proc.Memory.SetMemoryRegion(uint64 teb.Self, tebRegion)
        
        uint64 teb.Self

    let createTeb64(sandbox: BaseSandbox) =
        let proc = sandbox.GetRunningProcess()
        
        // create the TEB and fix address if necessary. 
        // I multiple by 2 just to reserve enough space and avoid to move it in the future
        let teb = buildTeb64(sandbox)
        let tebSize = Helpers.deepSizeOf(teb.GetType(), proc.PointerSize) * 2
        let tebAddress = proc.Memory.GetFreeMemory(tebSize, 0x7ff70000UL) 
        let teb = {teb with Self = tebAddress}

        // write TEB to memory
        proc.Memory.WriteMemory(teb.Self, teb)
        let tebRegion = {proc.Memory.GetMemoryRegion(teb.Self) with Info = "TEB"}
        proc.Memory.SetMemoryRegion(teb.Self, tebRegion)
        
        teb.Self

