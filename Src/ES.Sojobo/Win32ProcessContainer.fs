namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2
open B2R2.FrontEnd
open B2R2.BinFile
open ES.Sojobo.Model
open B2R2.FrontEnd.Intel

type Win32ProcessContainer() as this =  
    inherit BaseProcessContainer(32)

    let _memoryManager = new MemoryManager(32)
    let _iat = new List<Symbol>()
    let _cpu = new Cpu()
    
    let setEntryPoint(handler: BinHandler) =
        this.UpdateActiveMemoryRegion(_memoryManager.GetMemoryRegion(handler.FileInfo.EntryPoint))

        // save the EIP registry value
        let eipValue = createVariableWithValue(string Register.EIP, EmulatedType.DoubleWord, BitVector.ofUInt64 handler.FileInfo.EntryPoint 32<rt>)
        this.Cpu.SetRegister(eipValue)

    let setupRegisters() =
        [
            // segments
            createVariableWithValue(string Register.SS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.SSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.CSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.DSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ES, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.ESBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.FS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.FSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.GS, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)
            createVariableWithValue(string Register.GSBase, EmulatedType.DoubleWord, BitVector.ofUInt32 0ul 32<rt>)

            // general purpose registers
            createVariableWithValue(string Register.EAX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EBX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ECX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDX, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.ESI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)
            createVariableWithValue(string Register.EDI, EmulatedType.DoubleWord, BitVector.ofUInt32 0u 32<rt>)

            // flag registers
            createVariableWithValue(string Register.OF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.DF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.IF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.TF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.SF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.ZF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.AF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.PF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
            createVariableWithValue(string Register.CF, EmulatedType.Bit, BitVector.ofUInt32 0u 1<rt>)
        ] |> List.iter(fun register -> this.Cpu.SetRegister(register))

    let setupStackRegisters() =
        // set ESP value
        let startAddress = int32 _memoryManager.Stack.BaseAddress + int32 _memoryManager.Stack.Content.Length - 8
        let espValue = createVariableWithValue(string Register.ESP, EmulatedType.DoubleWord, BitVector.ofInt32 startAddress 32<rt>)
        this.Cpu.SetRegister(espValue)

        // set EBP value equals to ESP
        let ebpValue = createVariableWithValue(string Register.EBP, EmulatedType.DoubleWord, espValue.Value)
        this.Cpu.SetRegister(ebpValue)
        
    let resolveIATSymbols(handler: BinHandler) =
        handler.FileInfo.GetSymbols()
        |> Seq.iter(fun symbol ->
            if 
                not(String.IsNullOrEmpty(symbol.LibraryName)) && 
                (symbol.Kind = SymbolKind.ExternFunctionType || symbol.Kind = SymbolKind.FunctionType) 
            then 
                _iat.Add(symbol)
        )

    let initialize(handler: BinHandler) =
        Utility.mapPeHeader(handler, _memoryManager)
        Utility.mapSections(handler, _memoryManager)
        setupStackRegisters()
        setEntryPoint(handler)
        setupRegisters()
        resolveIATSymbols(handler)
    
    default this.Memory = _memoryManager
    default this.Cpu = _cpu
        
    member this.Initialize(buffer: Byte array) =
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, buffer)
        initialize(handler)

    member this.Initialize(filename: String) =  
        let isa = ISA.OfString "x86"
        let handler = BinHandler.Init(isa, ArchOperationMode.NoMode, true, Addr.MinValue, filename)        
        initialize(handler)

    default this.GetImportedFunctions() =
        Seq.readonly _iat
        
    default this.GetInstruction() =
        let programCounter = this.ProgramCounter.Value |> BitVector.toUInt64
        BinHandler.ParseInstr (this.GetActiveMemoryRegion().Handler) (programCounter)

    default this.ProgramCounter
        with get() = this.Cpu.GetRegister("EIP")

    default this.GetCallStack() = [|
        let mutable ebp = this.Cpu.GetRegister("EBP").Value |> BitVector.toUInt32
        let mutable retValue = BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
        while retValue <> 0ul do
            yield uint64 retValue
            ebp <- BitConverter.ToUInt32(this.Memory.ReadMemory(uint64 ebp, 4) , 0)
            retValue <- BitConverter.ToUInt32(this.Memory.ReadMemory(ebp + 4ul |> uint64, 4) , 0)
    |]