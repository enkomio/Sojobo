namespace ES.Sojobo

open B2R2.BinIR.LowUIR
open B2R2.FrontEnd

type ILowUIREmulator =
    interface
        /// Emulates the input instruction considering the given binary handle
        abstract EmulateInstruction: BinHandler * Instruction -> unit

        /// Emulates the input statements
        abstract Emulate: Stmt array -> unit  
    end

