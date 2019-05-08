namespace ES.Sojobo

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open B2R2.FrontEnd
open B2R2
open ES.Sojobo.Model
open B2R2.BinIR

[<AbstractClass>]
type BaseSandbox() =
    let _sideEffectEvent = new Event<ISandbox * SideEffect>()

    abstract Create: String -> unit
    abstract Create: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer

    member this.SideEffect = _sideEffectEvent.Publish

    member internal this.TriggerSideEffect(sideEffect: SideEffect) =
        _sideEffectEvent.Trigger(upcast this, sideEffect)

    interface  ISandbox with
        member this.Create(filename: String) =
            this.Create(filename)

        member this.Create(buffer: Byte array) =
            this.Create(buffer)

        member this.Run() =
            this.Run()

        member this.Stop() =
            this.Stop()
            
        member this.GetRunningProcess() =
            this.GetRunningProcess()

        member this.SideEffect
            with get() = this.SideEffect