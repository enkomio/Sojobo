namespace ES.Sojobo

type Library =
    | Managed of assembly: ManagedLibrary
    | Native of library: NativeLibrary