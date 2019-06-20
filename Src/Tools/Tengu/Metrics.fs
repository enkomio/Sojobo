namespace ES.Tengu

open System
open System.Collections.Generic
open ES.Sojobo
open ES.Sojobo.Model
open B2R2.FrontEnd
open B2R2
open System.Text
open System.IO
open ES.Fslog

type internal MetricPoint = {
    InstructionCounter: Int32
    StackFrameCounter: Int32
    OperationType: String
    FunctionAddress: UInt64
}

type internal Metrics(sandbox: ISandbox) =
    let _data = new List<MetricPoint>()
    let mutable _stackFrameCounter = 1

    let _logger =
        log "Metrics"
        |> info "SavedMetrics" "Saved metrics to: {0}"
        |> build    

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

    member this.ConfigureLogger(logProvider: ILogProvider) =
        logProvider.AddLogSourceToLoggers(_logger)

    member this.SaveInformation() =
        let sb = new StringBuilder()
        sb.AppendLine("Instruction counter, Stack frame counter, Operation type, Function Address") |> ignore
        _data        
        |> Seq.iter(fun metric -> 
            sb
                .AppendFormat("{0}, {1}, {2}, {3}", 
                    metric.InstructionCounter, 
                    metric.StackFrameCounter,
                    metric.OperationType,
                    metric.FunctionAddress
                )
                .AppendLine() 
            |> ignore
        )
        let file = Path.Combine(Utility.getResultDir(sandbox.GetRunningProcess().Pid), "metrics_stack_frame.txt")
        File.WriteAllText(file, sb.ToString())
        _logger?SavedMetrics(file)