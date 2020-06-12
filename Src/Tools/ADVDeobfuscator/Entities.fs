namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd.Intel

module Entities =
    (*
    type Patch = {
        VirtualAddress: UInt64
        OldBytes: Byte array
        NewBytes: Byte array
    }
    *)

    type Function = {
        Address: UInt64
        Instructions: IntelInstruction array        
    } with
        member this.TryGetInstruction(address: UInt64) =
            this.Instructions
            |> Seq.tryFind(fun instruction -> instruction.Address = address)

        member this.GetPreviousInstruction(instruction: IntelInstruction) =
            this.Instructions
            |> Array.pairwise
            |> Array.tryFind(fun (_, cur) -> instruction.Address = cur.Address)
            |> function
                | Some (prev, _) -> prev
                | None -> instruction

    [<Flags>]
    type InstructionFlags =
        | Empty = 0
        | XorWith8BitRegister = 1
        | XorWithStack = 2
        | AddStackWithImmediate = 4
        | JumpBelow = 8
        | JumpToPreviousAddress = 16
        | SetByteRegisterInStack = 32
        | MovToStack = 64
        | MovdqaToStack = 128
        | CallToFunctionDecrypt = 256
        | JumpToLoopEdge = 512  
        | CallToExtraneousFunction = 1024

    let startFlags = [
        InstructionFlags.MovdqaToStack
        InstructionFlags.MovToStack
    ]

    let encryptionFlags = [
        InstructionFlags.XorWithStack
        InstructionFlags.XorWith8BitRegister
        InstructionFlags.AddStackWithImmediate
    ]

    let deobfuscationFlags = 
        InstructionFlags.CallToFunctionDecrypt::encryptionFlags 

    let terminationFlags = [
        InstructionFlags.SetByteRegisterInStack
        InstructionFlags.JumpToLoopEdge
        InstructionFlags.CallToExtraneousFunction
    ]

    let private _notDeobfuscationFlags = 
        deobfuscationFlags 
        |> List.fold(fun s f -> s ||| f) InstructionFlags.Empty
        |> (~~~)    

    let private _notTerminationFlags = 
        terminationFlags 
        |> List.fold(fun s f -> s ||| f) InstructionFlags.Empty
        |> (~~~)

    let startOperationFound(flags: InstructionFlags) =
        startFlags |> List.exists(flags.HasFlag)

    let encryptionOperationFound(flags: InstructionFlags) =
        encryptionFlags |> List.exists(flags.HasFlag)

    let deobfuscationOperationFound(flags: InstructionFlags) =
        InstructionFlags.CallToFunctionDecrypt::encryptionFlags |> List.exists(flags.HasFlag)

    let terminatingOperationFound(flags: InstructionFlags) =
        terminationFlags |> List.exists(flags.HasFlag)
    
    type ObfuscationTrace = {
        StartAddress: UInt64
        DeobfuscateOperationAddress: UInt64
        EndAddress: UInt64
        Flags: InstructionFlags
    } with
        static member Empty = {
            StartAddress = 0UL
            DeobfuscateOperationAddress = 0UL
            EndAddress = 0UL
            Flags = InstructionFlags.Empty
        }

        member this.StartOperationFound
            with get() = startOperationFound(this.Flags)

        member this.EncryptionOperationFound
            with get() = encryptionOperationFound(this.Flags)

        member this.DeobfuscationOperationFound
            with get() = deobfuscationOperationFound(this.Flags)

        member this.TerminatingOperationFound
            with get() = terminatingOperationFound(this.Flags)

        member this.IsCompleted
            with get() =
                this.StartAddress > 0UL && 
                this.EndAddress > 0UL && 
                this.DeobfuscateOperationAddress > 0UL &&
                (this.StartAddress <= this.DeobfuscateOperationAddress &&  this.EndAddress >= this.DeobfuscateOperationAddress)
                
        member this.IsInvalidState(instruction: IntelInstruction) =            
            instruction.Info.Opcode = Opcode.CALLFar ||
            (instruction.Info.Opcode = Opcode.CALLNear && not(this.Flags.HasFlag(InstructionFlags.CallToFunctionDecrypt))) ||
            (this.TerminatingOperationFound && this.StartAddress = 0UL)

        member this.AddFlags(flags: InstructionFlags) =
            // zero out specific flags according to the current state
            if this.DeobfuscationOperationFound then
                {this with Flags = this.Flags ||| flags}
            elif this.StartOperationFound then
                {this with Flags = (this.Flags ||| flags) &&& _notTerminationFlags}
            else
                {this with Flags = (this.Flags ||| flags) &&& _notDeobfuscationFlags &&& _notTerminationFlags}