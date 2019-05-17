namespace IntegrationTests

module Program =
    [<EntryPoint>]
    let main argv = 
        SerializationTests.``Serialize auto-referenced object``()
        0
