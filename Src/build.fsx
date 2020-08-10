// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref FakeBuild //"
#load ".fake/build.fsx/intellisense.fsx"

#r @"System.IO.Compression"
#r @"System.IO.Compression.FileSystem"

open System
open System.Text
open System.Reflection
open System.IO
open Fake
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.Core
open Fake.IO
 
// The name of the project
let project = "Sojobo"

// Short summary of the project
let summary = "A binary analysis framework."

// List of author names
let authors = "Enkomio"
    
// Build dir
let buildDir = "build"

// Extension to not include in release
let forbiddenExtensions = [".pdb"]

// project names
let fsharpProjects = 
    [
        ("ES.Sojobo", "ES.Sojobo")
        ("ES.Sojobo.ADVAPI32", "ES.Sojobo.ADVAPI32")
        ("ES.Sojobo.ApiMsWinCrtRuntime", "ES.Sojobo.ApiMsWinCrtRuntime")
        ("ES.Sojobo.Kernel32", "ES.Sojobo.Kernel32")
        ("ES.Sojobo.USERENV", "ES.Sojobo.USERENV")
        ("ES.Sojobo.VCRUNTIME140", "ES.Sojobo.VCRUNTIME140")
        ("ES.Sojobo.Windows", "ES.Sojobo.Windows")
        ("ES.Sojobo.Winsock", "ES.Sojobo.Winsock")
        (Path.Combine("Tools", "ADVDeobfuscator"), "ADVDeobfuscator")
    ] 
    |> List.map(fun (projDir, projName) -> (Path.Combine(__SOURCE_DIRECTORY__, projDir), projName))

let csharpProjects = 
    [
        ("ES.Sojobo.CSharp", "ES.Sojobo.CSharp")
    ]
    |> List.map(fun (projDir, projName) -> (Path.Combine(__SOURCE_DIRECTORY__, projDir), projName))

// Read additional information from the release notes document
let releaseNotesData = 
    let changelogFile = Path.Combine(__SOURCE_DIRECTORY__, "..", "RELEASE_NOTES.md")
    File.ReadAllLines(changelogFile)
    |> ReleaseNotes.parseAll

// The effective Taipan Scanner release version
let releaseVersion = 
    let releaseNoteVersion = Version.Parse((List.head releaseNotesData).AssemblyVersion)
    let now = DateTime.UtcNow
    let timeSpan = now.Subtract(new DateTime(1980,2,1,0,0,0))
    let months = timeSpan.TotalDays / 30. |> int32
    let remaining = int32 timeSpan.TotalDays - months * 30
    string <| new Version(releaseNoteVersion.Major, releaseNoteVersion.Minor, months, remaining)    

Trace.trace("Build Version: " + releaseVersion)

// Targets
Target.create "Clean" (fun _ ->
    Fake.IO.Shell.cleanDirs [buildDir]
)

Target.create "SetAssemblyInfo" (fun _ ->
    fsharpProjects
    |> List.iter(fun (projDir, projName) ->
        let fileName = Path.Combine(projDir, "AssemblyInfo.fs")    
        AssemblyInfoFile.createFSharp fileName [         
            DotNet.AssemblyInfo.Title project
            DotNet.AssemblyInfo.Product project
            DotNet.AssemblyInfo.Guid "274BAD77-1F86-4CBC-A5B6-1FF724940C53"
            DotNet.AssemblyInfo.Company authors
            DotNet.AssemblyInfo.Description summary
            DotNet.AssemblyInfo.Version releaseVersion        
            DotNet.AssemblyInfo.FileVersion releaseVersion
            DotNet.AssemblyInfo.InformationalVersion releaseVersion
            DotNet.AssemblyInfo.Metadata("BuildDate", DateTime.UtcNow.ToString("yyyy-MM-dd")) 
        ]
    )    

    csharpProjects
    |> List.iter(fun (projDir, projName) ->
        let fileName = Path.Combine(projDir, "AssemblyInfo.cs")    
        AssemblyInfoFile.createCSharp fileName [         
            DotNet.AssemblyInfo.Title project
            DotNet.AssemblyInfo.Product project
            DotNet.AssemblyInfo.Guid "274BAD77-1F86-4CBC-A5B6-1FF724940C53"
            DotNet.AssemblyInfo.Company authors
            DotNet.AssemblyInfo.Description summary
            DotNet.AssemblyInfo.Version releaseVersion        
            DotNet.AssemblyInfo.FileVersion releaseVersion
            DotNet.AssemblyInfo.InformationalVersion releaseVersion
            DotNet.AssemblyInfo.Metadata("BuildDate", DateTime.UtcNow.ToString("yyyy-MM-dd")) 
        ]
    )    
)

Target.create "Compile" (fun _ ->
    let compile(projects: (String * String) list, extension: String) =
        projects
        |> List.iter(fun (projDir, projectName) ->
            let project = Path.Combine(projDir, projectName + extension)        
            let buildAppDir = Path.Combine(buildDir, projectName)
            Fake.IO.Directory.ensure buildAppDir

            // compile
            DotNet.MSBuild.runRelease id buildAppDir "Build" [project]
            |> Console.WriteLine
        )

    compile(fsharpProjects, ".fsproj")
    compile(csharpProjects, ".csproj")
)

Target.create "CleanBuild" (fun _ ->
    Directory.GetFiles(buildDir, "*.*", SearchOption.AllDirectories)  
    |> Array.filter(fun file ->
        forbiddenExtensions
        |> List.contains (Path.GetExtension(file).ToLowerInvariant())
    )
    |> Array.iter(File.Delete)
)

Target.create "Release" (fun _ ->
    // create zip file for projectx      
    fsharpProjects@csharpProjects
    |> List.filter(fun (projDir, projName) -> projName.StartsWith("ES.") |> not)
    |> List.iter(fun (projDir, projName) ->         
        let releaseFilename = String.Format("{0}.v{1}.zip", projName, releaseVersion)        
        Directory.GetFiles(Path.Combine(buildDir, projName), "*.*", SearchOption.AllDirectories)
        |> Fake.IO.Zip.zip (Path.Combine(buildDir, projName)) (Path.Combine(buildDir, releaseFilename))
        
    )
)

"Clean"        
    ==> "SetAssemblyInfo"
    ==> "Compile" 
    ==> "CleanBuild"
    ==> "Release"
    
// Start build
Core.Target.runOrDefault "Release"