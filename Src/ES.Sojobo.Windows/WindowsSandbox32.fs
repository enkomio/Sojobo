namespace ES.Sojobo.Windows


type WindowsSandbox32(settings: WindowsSandboxSettings) =
    inherit WindowsSandbox(32, settings)

    new() = new WindowsSandbox32(WindowsSandboxSettings.Default) 