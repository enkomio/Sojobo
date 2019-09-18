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

    abstract Load: String -> unit
    abstract Load: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer
    abstract ResetProcessState: unit -> unit
    abstract GetHookAddress: Hook -> UInt64 option

    member this.SideEffect = _sideEffectEvent.Publish
    member val internal Libraries = new List<Library>() with get
    member val Emulator: IEmulator option = None with get, set
    member val internal Hooks = new List<Hook>() with get
    member val Id = Guid.NewGuid()

    abstract GetLibraries: unit -> Library array
    default this.GetLibraries() =
        this.Libraries 
        |> Seq.readonly 
        |> Seq.toArray
    
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

    member this.AddLibrary(assembly: Assembly) =
        let library = new ManagedLibrary(assembly, this.Emulator.Value)
        this.Libraries.Add(Managed library)

    member this.AddLibrary(content: Byte array) = 
        try
            // first try to load the library as an Assembly
            let assembly = Assembly.Load(content)
            this.AddLibrary(assembly)
        with
            | :? BadImageFormatException ->
                this.Libraries.Add(Native <| NativeLibrary.Create(content))    

    abstract AddLibrary: filename:String -> unit
    default this.AddLibrary(filename: String) =
        try
            // first try to load the library as an Assembly
            let assembly = Assembly.LoadFile(Path.GetFullPath(filename))
            this.AddLibrary(assembly)
        with :? BadImageFormatException ->
            this.Libraries.Add(Native <| NativeLibrary.Create(filename)) 

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

        member this.AddLibrary(assembly: Assembly) =
            this.AddLibrary(assembly)

        member this.AddLibrary(filename: String) =
            this.AddLibrary(filename)

        member this.AddLibrary(content: Byte array) =
            this.AddLibrary(content)

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
        member this.SideEffect
            with get() = this.SideEffect

        member this.Emulator
            with get() = this.Emulator.Value

        member this.Id
            with get() = this.Id