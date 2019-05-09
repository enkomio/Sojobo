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

    abstract Load: String -> unit
    abstract Load: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer

    member this.SideEffect = _sideEffectEvent.Publish

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

        member this.SideEffect
            with get() = this.SideEffect