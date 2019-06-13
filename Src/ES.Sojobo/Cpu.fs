namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2

type Cpu() =
    let _variables = new Dictionary<String, EmulatedValue>()
    let _tempVariables = new Dictionary<String, EmulatedValue>()

    do
        [
            // segments
            createVariableWithValue(string Register.SS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.SSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ES, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ESBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.GS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.GSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)

            // general purpose registers
            createVariableWithValue(string Register.EAX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EBX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ECX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ESI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)

            // flag registers
            createVariableWithValue(string Register.OF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.DF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.IF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.TF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.SF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.ZF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.AF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.PF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.CF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
        ] |> List.iter(fun register -> _variables.[register.Name] <- register)

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