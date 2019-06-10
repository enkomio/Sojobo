// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref FakeBuild //"
#load ".fake/build.fsx/intellisense.fsx"

#r @"System.IO.Compression"
#r @"System.IO.Compression.FileSystem"

open System
open System.Text
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

// Package dir
let releaseDir = "binaries"

// project names
let fsharpProjects = [
    "ES.Sojobo"
    "ES.Sojobo.ADVAPI32"
    "ES.Sojobo.ApiMsWinCrtRuntime"
    "ES.Sojobo.Kernel32"
    "ES.Sojobo.USERENV"
    "ES.Sojobo.VCRUNTIME140"
]

let csharpProjects = [
    "ES.Sojobo.CSharp"
]

// set the script dir as current
Directory.SetCurrentDirectory(__SOURCE_DIRECTORY__)

// Read additional information from the release notes document
let releaseNotesData = 
    let changelogFile = Path.Combine("..", "RELEASE_NOTES.md")
    File.ReadAllLines(changelogFile)
    |> ReleaseNotes.parse

let releaseNoteVersion = Version.Parse(releaseNotesData.AssemblyVersion)
let now = DateTime.UtcNow
let timeSpan = now.Subtract(new DateTime(1980,2,1,0,0,0))
let remaining = timeSpan.TotalDays % 30. |> int32
let months = timeSpan.TotalDays / 30. |> int32
let releaseVersion = string <| new Version(releaseNoteVersion.Major, releaseNoteVersion.Minor, months, now.Day + remaining)
Trace.trace("Build Version: " + releaseVersion)

// Targets
Core.Target.create "Clean" (fun _ ->
    Fake.IO.Shell.cleanDirs [buildDir; releaseDir]
)

Core.Target.create "SetAssemblyInfo" (fun _ ->
    fsharpProjects
    |> List.iter(fun projName ->
        let fileName = Path.Combine(projName, "AssemblyInfo.fs")    
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
    |> List.iter(fun projName ->
        let fileName = Path.Combine(projName, "AssemblyInfo.cs")    
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

Core.Target.create "Compile" (fun _ ->
    fsharpProjects
    |> List.iter(fun projectName ->
        let project = Path.Combine(projectName, projectName + ".fsproj")        
        let buildAppDir = Path.Combine(buildDir, projectName)
        Fake.IO.Directory.ensure buildAppDir

        // compile
        DotNet.MSBuild.runRelease id buildAppDir "Build" [project]
        |> Trace.logItems "Build Output: "
    )

    csharpProjects
    |> List.iter(fun projectName ->
        let project = Path.Combine(projectName, projectName + ".csproj")        
        let buildAppDir = Path.Combine(buildDir, projectName)
        Fake.IO.Directory.ensure buildAppDir

        // compile
        DotNet.MSBuild.runRelease id buildAppDir "Build" [project]
        |> Trace.logItems "Build Output: "
    )
)

Core.Target.create "Release" (fun _ ->
    let releaseDirectory = Path.Combine(releaseDir, String.Format("ES.Sojobo.v{0}", releaseVersion))
    Directory.CreateDirectory(releaseDirectory) |> ignore

    // copy all files in the release dir    
    fsharpProjects@csharpProjects
    |> List.map(fun projName -> Path.Combine(buildDir, projName))
    |> List.iter(fun projDir -> 
        Shell.copyDir releaseDirectory projDir (fun _ -> true)
    )
    
    // create zip file
    let buildDirectory = Path.Combine(buildDir, "ES.Sojobo")
    let releaseFilename = releaseDirectory + ".zip"
    Directory.GetFiles(releaseDirectory, "*.*", SearchOption.AllDirectories)
    |> Array.filter(fun file ->
        [".pdb"] 
        |> List.contains (Path.GetExtension(file).ToLowerInvariant())
        |> not
    )
    |> Fake.IO.Zip.zip releaseDirectory releaseFilename
)

"Clean"        
    ==> "SetAssemblyInfo"
    ==> "Compile" 
    ==> "Release"
    
// Start build
Core.Target.runOrDefault "Release"