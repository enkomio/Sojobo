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
    let _libraries = new List<Library>()
    let _libraryFunctions = new Dictionary<String, MethodInfo>()
    let _callbacks = new Dictionary<UInt64, String>()      

    abstract Load: String -> unit
    abstract Load: Byte array -> unit  
    abstract Run: unit -> unit
    abstract Stop: unit -> unit     
    abstract GetRunningProcess: unit -> IProcessContainer

    member this.SideEffect = _sideEffectEvent.Publish
    member internal this.Callbacks = _callbacks
    member internal this.LibraryFunctions = _libraryFunctions
    member internal this.Libraries = _libraries

    member this.AddLibrary(assembly: Assembly) =
        this.Libraries.Add(Assembly assembly) 

    member this.AddLibrary(content: Byte array) =
        this.Libraries.Add(Native content) 

    member this.AddLibrary(filename: String) =
        this.Libraries.Add(File filename) 

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