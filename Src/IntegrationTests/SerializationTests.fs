namespace IntegrationTests

open System
open B2R2.BinFile
open ES.Sojobo.Windows
open ES.Sojobo.Windows.WindowsStructures

module SerializationTests =
    let ``[Test] Serialize an Ldr structure``() =
        let sandbox = new WindowsSandbox({Win32SandboxSettings.Default with InitializeEnvironment = false})
        sandbox.Load(Helper.getTestFile("help.exe"))
        sandbox.MapLibrary(Helper.getTestFullPath("kernel32.dll"))
        sandbox.MapLibrary(Helper.getTestFullPath("msvcrt.dll"))
        sandbox.MapLibrary(Helper.getTestFullPath("ntdll.dll"))
        sandbox.MapLibrary(Helper.getTestFullPath("KernelBase.dll"))

        let ldr = WindowsStructures.buildPeb32(sandbox).Ldr
        let proc = sandbox.GetRunningProcess()
        let address = proc.Memory.AllocateMemory(0x10000, Permission.Readable)
        
        // write object to memory and read result
        proc.Memory.WriteMemory(address, ldr)
        let loadedLdr = proc.Memory.ReadMemory<PEB_LDR_DATA>(address)

        // Check that the first and last dll base is the same
        assert(loadedLdr.InInitializationOrderLinks.Forward.DllBase = ldr.InInitializationOrderLinks.Forward.DllBase)
        assert(loadedLdr.InInitializationOrderLinks.Backward.DllBase = ldr.InInitializationOrderLinks.Backward.DllBase)