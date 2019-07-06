namespace ES.Sojobo

open B2R2.BinIR.LowUIR
open B2R2.FrontEnd

type IEmulator =
    interface
        /// Emulates the input instruction considering the given binary handle and advance program counter if necessary
        abstract Emulate: BinHandler * Instruction -> Stmt array

        /// Emulates the input statements
        abstract Emulate: Stmt array -> unit  

        /// Emulates the input instruction considering the given binary handle
        abstract EmulateInstruction: BinHandler * Instruction -> Stmt array

        /// Advance the Program Counter according to the input instruction
        abstract AdvanceProgramCounterIfNecessary: Instruction -> unit
    end

