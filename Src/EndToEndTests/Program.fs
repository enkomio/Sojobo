namespace ES.EndToEndTests

module Program =
    [<EntryPoint>]
    let main argv =
        //AnalysisTests.``graph generation``()

        SnapshotTests.``test snapshot creation and loading``()

        DumpDynamicMemory.``dump dynamically executed memory``()
        DumpDynamicMemory.``dump freed memory by using hooks``()
        0
