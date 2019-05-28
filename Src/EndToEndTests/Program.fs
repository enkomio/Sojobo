namespace ES.EndToEndTests

module Program =
    [<EntryPoint>]
    let main argv =
        DumpDynamicMemory.``dump dynamically executed memory``()
        DumpDynamicMemory.``dump freed memory by using hooks``()
        0
