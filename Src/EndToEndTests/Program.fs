namespace ES.EndToEndTests

module Program =
    [<EntryPoint>]
    let main argv =
        DumpDynamicMemory.``dump dynamically executed memory``()
        0
