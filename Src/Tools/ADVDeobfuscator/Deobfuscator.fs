namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd
open B2R2
open ES.ADVDeobfuscator.Entities
open ES.Fslog
open B2R2.FrontEnd.Intel

type Deobfuscator(fileName: String, numInstructionsToEmulate: Int32, logProvider: ILogProvider) =   
    let _binHandler = BinHandler.Init(ISA.DefaultISA, ArchOperationMode.NoMode, true, Addr.MinValue, fileName)
    let _id = 
        let now = DateTime.Now        
        String.Format(
            "{0}{1}{2}_{3}{4}{5}", 
            now.Year, 
            now.Month, 
            now.Day, 
            now.Hour.ToString("00"),
            now.Minute.ToString("00"),
            now.Second.ToString("00")
        )
        
    let _logger =
        log "Deobfuscator"
        |> info "Start" "Start analyzed identified functions"
        |> verbose "AnalyzeInstructions" "Analyze instructions at: 0x{0}, instructions count: {1}"
        |> verbose "StringFromFunc" "Extracted {0} strings from function: 0x{1}"
        |> verbose "TraceCompleted" "Heuristic matched. Start: 0x{0}, Obfuscation operation: 0x{1}, End: 0x{2}"
        |> info "TotalStringExtracted" "Extracted a total of {0} strings"
        |> buildAndAdd(logProvider)
    
    let analyzeInstructions(functionsWithEncryption: UInt64 array, func: Function, startAddress: UInt64, instructions: IntelInstruction array) =
        let mutable numOfStringExtracted = 0
        let heuristics = new HeuristicAggregator(functionsWithEncryption, logProvider)        
        _logger?AnalyzeInstructions(startAddress.ToString("X"), instructions.Length)
                
        instructions
        |> Seq.iter(fun instruction ->
            heuristics.AnalyzeInstruction(func, instruction)
            if heuristics.IsSatisfied() then
                let trace = heuristics.GetTrace(func)                
                heuristics.Reset()

                _logger?TraceCompleted(trace.StartAddress.ToString("X"), trace.DeobfuscateOperationAddress.ToString("X"), trace.EndAddress.ToString("X"))
                let emulatorHeuristics = new HeuristicAggregator(functionsWithEncryption, logProvider)  
                let emulator = new Emulator(_id, fileName, func, emulatorHeuristics, numInstructionsToEmulate, logProvider)
                if emulator.Emulate(trace) then
                    numOfStringExtracted <- numOfStringExtracted + 1            
        )

        numOfStringExtracted

    member this.DeobfuscateAddress(address: UInt64) =
        let binScanner = new BinaryScanner(_binHandler, logProvider)
        let functionsWithEncryption = binScanner.GetFunctionsWithEncryption()
        let functionAddressesWithEncryption =
            functionsWithEncryption
            |> Array.map(fun func -> func.StartAddress)

        _logger?Start()
        let mutable totalStringExtracted = 0
        functionsWithEncryption
        |> Array.tryFind(fun func -> func.StartAddress <= address && address < func.EndAddress)
        |> Option.iter(fun func -> 
            let instructions = 
                func.Instructions
                |> Array.skipWhile(fun instruction -> instruction.Address <> address)

            let numStringExtracted = analyzeInstructions(functionAddressesWithEncryption, func, address, instructions)
            totalStringExtracted <- totalStringExtracted + numStringExtracted
            _logger?StringFromFunc(numStringExtracted, func.StartAddress.ToString("X"))
        )
        _logger?TotalStringExtracted(totalStringExtracted)

    member this.Deobfuscate() =
        let binScanner = new BinaryScanner(_binHandler, logProvider)
        let functionsWithEncryption = binScanner.GetFunctionsWithEncryption()
        let functionAddressesWithEncryption =
            functionsWithEncryption
            |> Array.map(fun func -> func.StartAddress)

        _logger?Start()
        let mutable totalStringExtracted = 0
        functionsWithEncryption
        |> Array.iter(fun func -> 
            let numStringExtracted = analyzeInstructions(functionAddressesWithEncryption, func, func.StartAddress, func.Instructions)
            totalStringExtracted <- totalStringExtracted + numStringExtracted
            _logger?StringFromFunc(numStringExtracted, func.StartAddress.ToString("X"))
        )
        _logger?TotalStringExtracted(totalStringExtracted)