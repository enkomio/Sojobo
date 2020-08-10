namespace ES.Tengu

open System
open ES.Fslog
open ES.Tengu.Cli

module Program =

    [<EntryPoint>]
    let main argv = 
        Cli.printBanner()        
        match getSettings(argv) with
        | Some settings->
            let tengu = new Tengu(settings)
            tengu.Run()
            0
        | None -> 
            1