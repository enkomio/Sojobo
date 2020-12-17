namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd
open B2R2.BinCorpus
open B2R2.BinGraph
open B2R2.FrontEnd.Intel
open ES.ADVDeobfuscator.Entities
open ES.Fslog

type BinaryScanner(binHandler: BinHandler, logProvider: ILogProvider) =
    let _logger =
        log "BinaryScanner"
        |> info "Start" "Start to identify interesting functions"
        |> verbose "FunctionAt" "Identified interesting function at: 0x{0}"
        |> buildAndAdd(logProvider)       

    let retrieveAllFunctions() =     
        let apparatus = Apparatus.init binHandler
        let scfg = SCFG (binHandler, apparatus)
        Apparatus.getInternalFunctions apparatus
        |> Seq.choose(fun callee ->
            callee.Addr
            |> Option.map(fun address ->
                let instructions =
                    let cfg, _ = scfg.GetFunctionCFG address                    
                    cfg.GetVertices()
                    |> Seq.collect(fun vertex ->
                        vertex.VData.GetInstructions()
                        |> Seq.cast<IntelInstruction>
                    )
                    |> Seq.toArray
                {
                    Architecture = (if binHandler.ISA.Arch = B2R2.Architecture.IntelX86 then Arch.X86 else Arch.X64)
                    StartAddress = address
                    Instructions = instructions
                    EndAddress = (instructions |> Array.last).Address
                }                
            )
        )
        |> Seq.toArray
        
    let retrieveFunctionsToAnalyze(functions: Function array, monitoredFunctions: UInt64 array) =        
        functions
        |> Array.filter(fun func -> func.Instructions.Length > 3)
        |> Array.filter(fun func ->
            let heuristics = new HeuristicAggregator(monitoredFunctions, logProvider)
            heuristics.IgnorePrecondition <- true
            func.Instructions
            |> Array.exists(fun instruction -> 
                heuristics.AnalyzeInstruction(func, instruction)
                heuristics.DeobfuscationFound()
            )
        )        
        
    member this.GetFunctionsWithEncryption() =
        _logger?Start()
        let allFunctions = retrieveAllFunctions()        

        // first step identify all functions that contain a deobfuscation operation
        let functionAddressesWithDeobfuscationOperationOnly = 
            retrieveFunctionsToAnalyze(allFunctions, Array.empty)
            |> Array.map(fun f -> f.StartAddress)

        // now, I know which functions have deobfuscation operations, 
        // re-create the heuristic with this additional info
        let functionsToAnalyze = retrieveFunctionsToAnalyze(allFunctions, functionAddressesWithDeobfuscationOperationOnly)
        
        functionsToAnalyze
        |> Array.iter(fun func -> _logger?FunctionAt(func.StartAddress.ToString("X")))
        
        functionsToAnalyze