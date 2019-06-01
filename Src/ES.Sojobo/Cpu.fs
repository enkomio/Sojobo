namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model

type Cpu() =
    let _variables = new Dictionary<String, EmulatedValue>()
    let _tempVariables = new Dictionary<String, EmulatedValue>()

    member internal this.GetTemporaryVariable(name: String) =
        _tempVariables.[name]

    member internal this.GetVariable(name: String) =
        _variables.[name]

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = Helpers.getTempName(index, emuType)
        match _tempVariables.TryGetValue(name) with
        | (true, value) -> value
        | _ -> 
            let variable = {createVariable(name, emuType) with IsTemp = true}
            _tempVariables.[name] <- variable
            variable    

    member internal this.GetVariable(name: String, emuType: EmulatedType) =        
        match _variables.TryGetValue(name) with
        | (true, value) -> value
        | _ ->
            let name = Helpers.getTempName(name, emuType)
            _tempVariables.[name]

    member internal this.ClearTemporaryVariables() =
        _tempVariables.Clear()

    member internal this.AddTemporaryVariable(name: string, value: EmulatedValue) =
        _tempVariables.[name] <- value

    member internal this.TryGetTemporaryVariable(name: String) =
        match _tempVariables.TryGetValue(name) with
        | (true, value) -> Some value
        | _ -> None

    member internal this.TryGetVariable(name: String) =
        match _variables.TryGetValue(name) with
        | (true, value) -> Some value
        | _ -> None

    member internal this.SetVariable(register: EmulatedValue) =
        _variables.[register.Name] <- register

    member internal this.GetAllVariables() =
        new Dictionary<String, EmulatedValue>(_variables)

    member this.GetRegister(name: String) =
        _variables.[name]

    member this.SetRegister(value: EmulatedValue) =
        if value.IsTemp
        then _tempVariables.[value.Name] <- value
        else _variables.[value.Name] <- value