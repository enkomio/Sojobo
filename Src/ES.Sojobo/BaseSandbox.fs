namespace ES.Sojobo

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open B2R2.BinIR
open B2R2

[<AbstractClass>]
type BaseSandbox() =
    let _sideEffectEvent = new Event<ISandbox * SideEffect>()
    let _beforeEmulationEvent = new Event<IProcessContainer>()   
    let _afterEmulationEvent = new Event<IProcessContainer>()

    abstract Load: String -> unit
    abstract Load: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer
    abstract ResetProcessState: unit -> unit
    abstract GetHookAddress: Hook -> UInt64 option

    member this.SideEffect = _sideEffectEvent.Publish
    member val ApiEmulators = new List<ApiEmulator>() with get
    member val NativeLibraries = new List<NativeLibrary>() with get
    member val Emulator: IEmulator option = None with get, set
    member val Hooks = new List<Hook>() with get
    member val Id = Guid.NewGuid()
    member this.BeforeEmulation = _beforeEmulationEvent.Publish 
    member this.AfterEmulation = _afterEmulationEvent.Publish 

    member this.SignalBeforeEmulation() =
        _beforeEmulationEvent.Trigger(this.GetRunningProcess())

    member this.SignalAfterEmulation() =
        _afterEmulationEvent.Trigger(this.GetRunningProcess())
        
    abstract AddHook: address:UInt64 * callback:Action<ISandbox> -> Hook
    default this.AddHook(address: UInt64, callback: Action<ISandbox>) =
        let hook = Address(address, callback)
        this.Hooks.Add(hook)
        hook

    abstract AddHook: symbol:String * callback:Action<ISandbox> -> Hook
    default this.AddHook(symbol: String, callback: Action<ISandbox>) =
        let hook = Symbol(symbol, callback)
        this.Hooks.Add(hook)
        hook

    abstract RemoveHook: hook:Hook -> unit
    default this.RemoveHook(hook: Hook) =
        while this.Hooks.Remove(hook) do ()

    member this.GetHooks() =
        this.Hooks |> Seq.toArray

    member this.AddApiEmulator(assembly: Assembly) =
        let library = new ApiEmulator(assembly, this.Emulator.Value)
        this.ApiEmulators.Add(library)

    member this.MapLibrary(content: Byte array) = 
        this.NativeLibraries.Add(NativeLibrary.Create(content))

    abstract MapLibrary: filename:String -> unit
    default this.MapLibrary(filename: String) =
        this.NativeLibraries.Add(NativeLibrary.Create(filename))

    member internal this.TriggerSideEffect(sideEffect: SideEffect) =
        _sideEffectEvent.Trigger(upcast this, sideEffect)

    interface  ISandbox with
        member this.Load(filename: String) =
            this.Load(filename)

        member this.Load(buffer: Byte array) =
            this.Load(buffer)

        member this.Run() =
            this.Run()

        member this.Stop() =
            this.Stop()
            
        member this.GetRunningProcess() =
            this.GetRunningProcess()

        member this.AddApiEmulator(assembly: Assembly) =
            this.AddApiEmulator(assembly)

        member this.MapLibrary(filename: String) =
            this.MapLibrary(filename)

        member this.MapLibrary(content: Byte array) =
            this.MapLibrary(content)

        member this.AddHook(address: UInt64, callback: Action<ISandbox>) =
            this.AddHook(address, callback)

        member this.AddHook(symbol: String, callback: Action<ISandbox>) =
            this.AddHook(symbol, callback)

        member this.RemoveHook(hook: Hook) =
            this.RemoveHook(hook)

        member this.GetHooks() =
            this.GetHooks()

        member this.GetHookAddress(hook: Hook) =
            this.GetHookAddress(hook)

        [<CLIEvent>]
        member this.BeforeEmulation
            with get() = this.BeforeEmulation

        [<CLIEvent>]
        member this.AfterEmulation
            with get() = this.AfterEmulation
            
        [<CLIEvent>]
        member this.SideEffect
            with get() = this.SideEffect

        member this.Emulator
            with get() = this.Emulator.Value

        member this.Id
            with get() = this.Id