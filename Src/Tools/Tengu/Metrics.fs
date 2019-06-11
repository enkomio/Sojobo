namespace ES.Tengu

open System
open System.Collections.Generic
open ES.Sojobo
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2

type internal MetricPoint = {
    InstructionCounter: Int32
    StackFrameCounter: Int32
    OperationType: String
    FunctionAddress: UInt64
}

type internal Metrics() =
    let _data = new List<MetricPoint>()
    let mutable _stackFrameCounter = 1

    let createEnterMetric(instrCounter: Int32, address: UInt64) = {
        InstructionCounter = instrCounter
        StackFrameCounter = _stackFrameCounter
        OperationType = "call"
        FunctionAddress = address
    }

    let createExitMetric(instrCounter: Int32, address: UInt64) =
        {createEnterMetric(instrCounter, address) with OperationType = "ret"}

    member this.EmulatedInstruction(proc: IProcessContainer, instrCounter: Int32) =
        let instruction = proc.GetInstruction()
        let pc = proc.ProgramCounter.Value |> BitVector.toUInt64

        if instruction.IsCall() then
            _stackFrameCounter <- _stackFrameCounter + 1
            _data.Add(createEnterMetric(instrCounter, pc))

        elif instruction.IsRET() then
            _stackFrameCounter <- _stackFrameCounter - 1
            _data.Add(createExitMetric(instrCounter, pc))

    member this.GetMetrics() =
        _data |> Seq.toArray