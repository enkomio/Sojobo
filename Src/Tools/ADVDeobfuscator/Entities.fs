namespace ES.ADVDeobfuscator

open System
open B2R2.FrontEnd.Intel

module Entities =
    type Arch =
        | X86
        | X64

    type Function = {
        StartAddress: UInt64
        EndAddress: UInt64
        Instructions: IntelInstruction array   
        Architecture: Arch
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
    type DeobfuscationFlag =
        | Empty = 0
        | XorWith8BitRegister = 1
        | XorWithStack = 2
        | AddWithImmediateOrRegister = 4
        | JumpToPreviousAddress = 16
        | SetByteRegisterInStack = 32
        | MovToStack = 64
        | MovToAVXRegister = 128
        | CallToFunctionDecrypt = 256
        | SubWithImmediateOrRegister = 2048

    type ObfuscationTrace = {
        Function: Function
        StartAddress: UInt64
        DeobfuscateOperationAddress: UInt64
        EndAddress: UInt64
    } with
        override this.ToString() =
            String.Format(
                "[Func:0x{0}] Start: 0x{1} - Obfuscation: 0x{2} - End: 0x{3}", 
                this.Function.StartAddress.ToString("X"),
                this.StartAddress.ToString("X"),
                this.DeobfuscateOperationAddress.ToString("X"),
                this.EndAddress.ToString("X")
            )