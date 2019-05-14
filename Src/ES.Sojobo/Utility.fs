namespace ES.Sojobo

open System
open System.Runtime.InteropServices
open ES.Sojobo.Model
open B2R2
open B2R2.FrontEnd
open B2R2.BinIR

module Utility =    

    let toArray(bitVector: BitVector) =
        let size = int32 <| BitVector.getType bitVector
        let value = BitVector.getValue bitVector
        match size with        
        | 8 -> [|byte value|]
        | 16 -> BitConverter.GetBytes(uint16 value)
        | 32 -> BitConverter.GetBytes(uint32 value)
        | 64 -> BitConverter.GetBytes(uint64 value)
        | _ -> failwith("Unexpected size: " + string size)

    let getType(regType: RegType) =
        match (RegType.toBitWidth regType) with
        | 1 -> EmulatedType.Bit
        | 8 -> EmulatedType.Byte
        | 16 -> EmulatedType.Word
        | 32 -> EmulatedType.DoubleWord
        | 64 -> EmulatedType.QuadWord
        | _ -> failwith("Invalid reg type size: " + regType.ToString())

    let getSize(emuType: EmulatedType) =
        match emuType with
        | EmulatedType.Bit -> 1
        | EmulatedType.Byte -> 8
        | EmulatedType.Word -> 16
        | EmulatedType.DoubleWord -> 32
        | EmulatedType.QuadWord -> 64

    let getTypeSize =
        getType >> getSize

    let formatCurrentInstruction(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        let disassembledInstruction = BinHandler.DisasmInstr handler false true instruction 
        let instructionBytes = BinHandler.ReadBytes(handler , instruction.Address, int32 instruction.Length)                
        let hexBytes = BitConverter.ToString(instructionBytes).Replace("-"," ")
        String.Format("0x{0,-10} {1, -30} {2}", instruction.Address.ToString("X") + ":", hexBytes, disassembledInstruction)
        
    let formatCurrentInstructionIR(processContainer: IProcessContainer) =
        let handler = processContainer.GetActiveMemoryRegion().Handler
        let instruction = processContainer.GetInstruction()
        BinHandler.LiftInstr handler instruction
        |> Array.map(LowUIR.Pp.stmtToString)

    let getTempName(index: String, emuType: EmulatedType) =
        let size =  getSize(emuType)
        String.Format("T_{0}:{1}", index, size)   