namespace ES.Sojobo

open System
open System.Collections.Generic
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel
open B2R2

type Cpu64() as this =
    inherit Cpu()

    do 
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

            // stack register
            createVariableWithValue(string Register.RBP, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.RSP, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
        ] |> List.iter(fun register -> this.Registers.[register.Name.ToUpperInvariant()] <- register)
