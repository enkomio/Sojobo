namespace IntegrationTests

open System
open B2R2.BinFile
open ES.Sojobo
open ES.Sojobo.Win32

module SerializationTests =

    let ``[Test] Serialize an Ldr structure``() =
        let sandbox = new Win32Sandbox({Win32SandboxSettings.Default with InitializeEnvironment = false})
        sandbox.Load(Helper.getTestFile("help.exe"))
        sandbox.AddLibrary(Helper.getTestFullPath("kernel32.dll"))
        sandbox.AddLibrary(Helper.getTestFullPath("msvcrt.dll"))

        let ldr = buildPeb(sandbox).Ldr
        let proc = sandbox.GetRunningProcess()
        let address = proc.Memory.AllocateMemory(0x10000, Permission.Readable)
        
        // write object to memory and read result
        proc.Memory.WriteMemory(address, ldr)
        let loadedLdr = proc.Memory.ReadMemory<PEB_LDR_DATA>(address)

        // Check that the first and last dll base is the same
        assert(loadedLdr.InInitializationOrderLinks.Forward.DllBase = ldr.InInitializationOrderLinks.Forward.DllBase)
        assert(loadedLdr.InInitializationOrderLinks.Backward.DllBase = ldr.InInitializationOrderLinks.Backward.DllBase)

