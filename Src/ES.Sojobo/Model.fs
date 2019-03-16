namespace ES.Sojobo

open System
open System.Collections.Generic
open B2R2.FrontEnd
open B2R2.BinFile

type MemoryProtection =
  | Read = 0
  | Write = 2
  | Execute = 4


type MemoryRegion = {
  BaseAddress: Int64

  Protection: SectionKind
  
  // if it is reserver/commit/etc...
  Type: String
  
  // contains additional information, like the name of the file mapped
  Info: String
  Data: Byte array
}