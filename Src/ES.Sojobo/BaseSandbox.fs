namespace ES.Sojobo

open System
open System.Reflection
open System.Collections.Generic
open B2R2.BinIR

[<AbstractClass>]
type BaseSandbox() =
    let _sideEffectEvent = new Event<ISandbox * SideEffect>()

    abstract Load: String -> unit
    abstract Load: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer

    member this.SideEffect = _sideEffectEvent.Publish
    member val internal Libraries = new List<Library>() with get
    member val internal Emulator: ILowUIREmulator option = None with get, set

    member this.AddLibrary(assembly: Assembly) =
        this.Libraries.Add(Managed <| new ManagedLibrary(assembly, this.Emulator.Value))

    member this.AddLibrary(content: Byte array) =
        this.Libraries.Add(Native <| NativeLibrary.Create(content)) 

    member this.AddLibrary(filename: String) =
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
        
        [<CLIEvent>]
        member this.SideEffect
            with get() = this.SideEffect