namespace IntegrationTests

open System
open System.Reflection

module Program =
    [<EntryPoint>]
    let main argv = 
        Assembly.GetExecutingAssembly().GetTypes()
        |> Seq.collect(fun t -> t.GetMethods())
        |> Seq.filter(fun m -> m.Name.StartsWith("[Test]", StringComparison.OrdinalIgnoreCase))
        |> Seq.iter(fun m -> m.Invoke(null, Array.empty) |> ignore)
        0
