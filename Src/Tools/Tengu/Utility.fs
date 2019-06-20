namespace ES.Tengu

open System
open System.IO
open System.Reflection

module Utility =
    let getResultDir(pid: UInt32) =
        let curDir = Path.GetDirectoryName(Assembly.GetEntryAssembly().Location)
        let directory = Path.Combine(curDir, "Result", "PID_" + pid.ToString())
        Directory.CreateDirectory(directory) |> ignore
        directory

