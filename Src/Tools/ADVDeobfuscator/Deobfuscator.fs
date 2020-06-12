namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd
open B2R2
open ES.ADVDeobfuscator.Entities
open ES.Fslog

type Deobfuscator(fileName: String, logProvider: ILogProvider) =   
    let _binHandler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, fileName)

    let _logger =
        log "Deobfuscator"
        |> info "Start" "Start analyzed identified functions"
        |> info "AnalyzeFunction" "Analyze function at: 0x{0}, instructions count: {1}"
        |> buildAndAdd(logProvider)
    
    let analyzeFunction(binScanner: BinaryScanner)(func: Function) =
        let mutable trace = ObfuscationTrace.Empty
        _logger?AnalyzeFunction(func.Address.ToString("X"), func.Instructions.Length)

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
                let emulator = new Emulator(fileName, binScanner, logProvider)
                emulator.Emulate(func, trace)
                trace <- ObfuscationTrace.Empty
                
            // check for invalid state
            elif trace.IsInvalidState(instruction) then
                trace <- ObfuscationTrace.Empty
        )

    member this.Deobfuscate() =
        let binScanner = new BinaryScanner(_binHandler, logProvider)
        _logger?Start()

        binScanner.Functions
        |> Array.iter(analyzeFunction binScanner)