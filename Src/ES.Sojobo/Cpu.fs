namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2

type Cpu() =
    let _variables = new Dictionary<String, EmulatedValue>()
    let _tempVariables = new Dictionary<String, EmulatedValue>()

    let createAvxRegisters(regName: String, emulatedType: EmulatedType, value: BitVector) =
        for i=0 to 15 do
            let registerName = String.Format("{0}{1}", regName, i)
            let registerValue = createVariableWithValue(registerName, emulatedType, value)
            _variables.[registerName.ToUpperInvariant()] <- registerValue

    do
        // segments
        [            
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
        ] |> List.iter(fun register -> _variables.[register.Name.ToUpperInvariant()] <- register)

        // 32-bit
        [   
            // general purpose registers
            createVariableWithValue(string Register.EIP, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
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
        ] |> List.iter(fun register -> _variables.[register.Name.ToUpperInvariant()] <- register)

        // 64-bit
        [   
            // general purpose registers
            createVariableWithValue(string Register.RIP, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RAX, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RBX, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RCX, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RDX, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RSI, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.RDI, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R8, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R9, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R10, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R11, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R12, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R13, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R14, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
            createVariableWithValue(string Register.R15, EmulatedType.QuadWord, BitVector.ofUInt32 0u 64<rt>)
        ] |> List.iter(fun register -> _variables.[register.Name.ToUpperInvariant()] <- register)

        // set AVX register
        createAvxRegisters("XMM", EmulatedType.XmmWord, BitVector.ofUInt32 0ul 128<rt>)
        createAvxRegisters("YMM", EmulatedType.YmmWord, BitVector.ofUInt32 0ul 256<rt>)
        createAvxRegisters("ZMM", EmulatedType.ZmmWord, BitVector.ofUInt32 0ul 512<rt>)
        
        // create the intermediate representation of ZMM
        for i=0 to 15 do
            for c in ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"] do
                let registerName = String.Format("ZMM{0}{1}", i, c)
                let registerValue = createVariableWithValue(registerName, EmulatedType.QuadWord, BitVector.ofUInt32 0ul 64<rt>)
                _variables.[registerName.ToUpperInvariant()] <- registerValue

    member internal this.GetTemporaryVariable(name: String) =
        _tempVariables.[name.ToUpperInvariant()]

    member internal this.GetVariable(name: String) =
        _variables.[name.ToUpperInvariant()]

    member internal this.GetOrCreateTemporaryVariable(index: String, emuType: EmulatedType) =
        let name = Helpers.getTempName(index, emuType)
        match _tempVariables.TryGetValue(name.ToUpperInvariant()) with
        | (true, value) -> value
        | _ -> 
            let variable = {createVariable(name, emuType) with IsTemp = true}
            _tempVariables.[name.ToUpperInvariant()] <- variable
            variable    

    member internal this.GetVariable(name: String, emuType: EmulatedType) =        
        match _variables.TryGetValue(name.ToUpperInvariant()) with
        | (true, value) -> value
        | _ ->
            let name = Helpers.getTempName(name, emuType)
            _tempVariables.[name.ToUpperInvariant()]

    member internal this.ClearTemporaryVariables() =
        _tempVariables.Clear()

    member internal this.AddTemporaryVariable(name: string, value: EmulatedValue) =
        _tempVariables.[name.ToUpperInvariant()] <- value

    member internal this.TryGetTemporaryVariable(name: String) =
        match _tempVariables.TryGetValue(name.ToUpperInvariant()) with
        | (true, value) -> Some value
        | _ -> None

    member internal this.TryGetVariable(name: String) =
        match _variables.TryGetValue(name.ToUpperInvariant()) with
        | (true, value) -> Some value
        | _ -> None

    member internal this.SetVariable(register: EmulatedValue) =
        _variables.[register.Name.ToUpperInvariant()] <- register

    member internal this.GetAllVariables() =
        new Dictionary<String, EmulatedValue>(_variables)

    member this.GetRegister(name: String) =
        _variables.[name.ToUpperInvariant()]

    member this.SetRegister(value: EmulatedValue) =
        let name = value.Name.ToUpperInvariant()
        if value.IsTemp
        then _tempVariables.[name] <- value
        else _variables.[name] <- value