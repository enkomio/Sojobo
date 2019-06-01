namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model

type Cpu() =
    member val internal Variables = new Dictionary<String, EmulatedValue>() with get
    member val internal TempVariables = new Dictionary<String, EmulatedValue>() with get
    (*
    static member internal GetTempName(index: String, emuType: EmulatedType) =
        let size =  getSize(emuType)
        "T_" + string index + ":" + string size

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = Utility.getTempName(index, emuType)
        match this.TempVariables.TryGetValue(name) with
        | (true, value) -> value
        | _ -> 
            let variable = {createVariable(name, emuType) with IsTemp = true}
            this.TempVariables.[name] <- variable
            variable    

    member internal this.GetVariable(name: String, emuType: EmulatedType) =        
        match this.Variables.TryGetValue(name) with
        | (true, value) -> value
        | _ ->
            let name = Utility.getTempName(name, emuType)
            this.TempVariables.[name]

    member internal this.ClearTemporaryVariables() =
        this.TempVariables.Clear()
        *)