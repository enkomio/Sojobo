namespace IntegrationTests

open System
open System.Reflection
open System.IO

module Helper =
    let getTestFullPath(name: String) =
        Path.Combine(
            Path.GetDirectoryName(Assembly.GetEntryAssembly().Location), 
            "..", "..", "..", "..", 
            "TestBinaries", 
            name
        )

    let getTestFile(name: String) =
        File.ReadAllBytes(getTestFullPath(name))
        

