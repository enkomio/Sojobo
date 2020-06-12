namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd
open B2R2.BinCorpus
open B2R2.BinGraph
open B2R2.FrontEnd.Intel
open ES.ADVDeobfuscator.Entities
open ES.Fslog

type BinaryScanner(binHandler: BinHandler, logProvider: ILogProvider) =
    let mutable _allFunctions = Array.empty<Function>
    let mutable _functionsToAnalyze = Array.empty<Function>
    let mutable _heuristics = new InstructionHeuristics(Array.empty)

    let _logger =
        log "BinaryScanner"
        |> info "Start" "Start to identify interesting functions"
        |> verbose "FunctionAt" "Identified interesting function at: 0x{0}"
        |> buildAndAdd(logProvider)

    let containsDeobfuscationOperation(func: Function) =
        func.Instructions
        |> Array.map(fun instr -> _heuristics.AnalyzeInstruction(func, instr))
        |> Array.exists(deobfuscationOperationFound)

    let retrieveAllFunctions() =     
        let apparatus = Apparatus.init binHandler
        let scfg = SCFG (binHandler, apparatus)
        Apparatus.getInternalFunctions apparatus
        |> Seq.choose(fun callee ->
            callee.Addr
            |> Option.map(fun address ->
                {
                    Address = address
                    Instructions =
                        let cfg, _ = scfg.GetFunctionCFG address                    
                        cfg.GetVertices()
                        |> Seq.collect(fun vertex ->
                            vertex.VData.GetInstructions()
                            |> Seq.cast<IntelInstruction>
                        )
                        |> Seq.toArray
                }                
            )
        )
        |> Seq.toArray

    let retrieveFunctionToAnalyze(functions: Function array) =
        functions
        |> Array.filter(fun func -> func.Instructions.Length > 3)
        |> Array.filter(containsDeobfuscationOperation)

    do
        _logger?Start()
        _allFunctions <- retrieveAllFunctions()

        // first step identify all function that contain a deobfuscation operation
        let functionsWithDeobfuscationOperationOnly = 
            retrieveFunctionToAnalyze(_allFunctions) 
            |> Array.map(fun f -> f.Address)
        _heuristics <- new InstructionHeuristics(functionsWithDeobfuscationOperationOnly)

        // now, I know which functions have deobfuscation operations, run again the scan to consider this new info
        _functionsToAnalyze <- retrieveFunctionToAnalyze(_allFunctions)
        let functionsToAnalyzeAddresses = _functionsToAnalyze |> Array.map(fun f -> f.Address)
        functionsToAnalyzeAddresses |> Array.iter(fun addr -> _logger?FunctionAt(addr.ToString("X")))

        // create the final object with full info
        _heuristics <- new InstructionHeuristics(functionsToAnalyzeAddresses)

    member this.Functions
        with get() = _functionsToAnalyze

    member this.AnalyzeInstruction(func: Function, instruction: IntelInstruction) =
        _heuristics.AnalyzeInstruction(func, instruction)