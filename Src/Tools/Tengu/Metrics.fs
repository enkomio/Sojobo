namespace ES.Tengu

open System
open System.Collections.Generic
open ES.Sojobo
open ES.Sojobo.Model
open B2R2.FrontEnd

type internal MetricPoint = {
    InstructionCounter: Int32
    StackFrameCounter: Int32
}

type internal Metrics() =
    let _data = new List<MetricPoint>()
    let mutable _stackFrameCounter = 1

    member this.EmulatedInstruction(instruction: Instruction, instrCounter: Int32) =
        if instruction.IsCall() then
            _stackFrameCounter <- _stackFrameCounter + 1
            _data.Add({InstructionCounter = instrCounter; StackFrameCounter = _stackFrameCounter})

        elif instruction.IsRET() then
            _stackFrameCounter <- _stackFrameCounter - 1
            _data.Add({InstructionCounter = instrCounter; StackFrameCounter = _stackFrameCounter})

    member this.GetMetrics() =
        _data |> Seq.toArray