namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model
open B2R2

module VCRUNTIME140 =
    let memset(sandbox: ISandbox, dest: UInt32, c: Int32, count: UInt32 ) =
        let winProcess = sandbox.GetRunningProcess()
        winProcess.WriteMemory(uint64 dest, Array.create<Byte> (int32 count) (byte c))
        let pointerSize = winProcess.GetPointerSize() |> LanguagePrimitives.Int32WithMeasure<rt>
        {ReturnValue = Some <| BitVector.ofUInt32 dest pointerSize; Convention = CallingConvention.Cdecl}