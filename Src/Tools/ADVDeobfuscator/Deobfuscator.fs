namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd
open B2R2
open ES.ADVDeobfuscator.Entities
open ES.Fslog

type Deobfuscator(fileName: String, numInstructionToEmulate: Int32, logProvider: ILogProvider) =   
    let _binHandler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, fileName)

    let _logger =
        log "Deobfuscator"
        |> info "Start" "Start analyzed identified functions"
        |> verbose "AnalyzeFunction" "Analyze function at: 0x{0}, instructions count: {1}"
        |> verbose "StringFromFunc" "Extracted {0} strings from function: 0x{1}"
        |> info "TotalStringExtracted" "Extracted a totla of {0} strings"
        |> buildAndAdd(logProvider)
    
    let analyzeFunction(binScanner: BinaryScanner) (func: Function) =
        let mutable trace = ObfuscationTrace.Empty
        let mutable numOfStringExtracted = 0
        _logger?AnalyzeFunction(func.Address.ToString("X"), func.Instructions.Length)

        let emulator = new Emulator(fileName, func, binScanner, numInstructionToEmulate, logProvider)
        func.Instructions
        |> Seq.iter(fun instruction ->
            // trace flags
            trace <- trace.AddFlags(binScanner.AnalyzeInstruction(func, instruction))

            if trace.StartAddress = 0UL && trace.StartOperationFound then
                trace <- {trace with StartAddress = instruction.Address}

            elif trace.StartAddress > 0UL && trace.DeobfuscateOperationAddress = 0UL && trace.DeobfuscationOperationFound then                
                trace <- {trace with DeobfuscateOperationAddress = instruction.Address}
                
            elif trace.DeobfuscateOperationAddress > 0UL && trace.EndAddress = 0UL && trace.TerminatingOperationFound then                
                trace <- {trace with EndAddress = instruction.Address}

            // emulate code if all info is retrived
            if trace.IsCompleted then                
                if emulator.Emulate(trace) then
                    numOfStringExtracted <- numOfStringExtracted + 1
                trace <- ObfuscationTrace.Empty
                
            // check for invalid state
            elif trace.IsInvalidState(instruction) then
                trace <- ObfuscationTrace.Empty
        )

        numOfStringExtracted

    member this.Deobfuscate() =
        let binScanner = new BinaryScanner(_binHandler, logProvider)
        _logger?Start()

        let mutable totalStringExtracted = 0
        binScanner.Functions
        |> Array.iter(fun func -> 
            let numStringExtracted = analyzeFunction binScanner func
            totalStringExtracted <- totalStringExtracted + numStringExtracted
            _logger?StringFromFunc(numStringExtracted, func.Address.ToString("X"))
        )
        _logger?TotalStringExtracted(totalStringExtracted)