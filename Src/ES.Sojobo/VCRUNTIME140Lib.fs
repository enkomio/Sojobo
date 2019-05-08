namespace ES.Sojobo.Lib

open System
open ES.Sojobo
open ES.Sojobo.Model

module VCRUNTIME140 =
    let memset(baseProcess: IProcessContainer, dest: UInt32, c: Int32, count: UInt32 ) =
        let cs = baseProcess.GetCallStack()
        let dest = baseProcess.GetArgument(0)
        let c = baseProcess.GetArgument(1)
        let count = baseProcess.GetArgument(2)

        Console.WriteLine("memset")
        {ReturnValue = None; Convention = CallingConvention.Cdecl}