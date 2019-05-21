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
 
// The name of the project
let project = "Sojobo"

// Short summary of the project
let summary = "A binary analysis framework."

// List of author names
let authors = "Enkomio"
    
// Build dir
let buildDir = "build"

// Package dir
let releaseDir = "binaries"

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
    let fileName = Path.Combine("ES.Sojobo", "AssemblyInfo.fs")    
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

Core.Target.create "Compile" (fun _ ->
    ["ES.Sojobo"]
    |> List.iter(fun projectName ->
        let project = Path.Combine(projectName, projectName + ".fsproj")        
        let buildAppDir = Path.Combine(buildDir, projectName)
        Fake.IO.Directory.ensure buildAppDir

        // compile
        DotNet.MSBuild.runRelease id buildAppDir "Build" [project]
        |> Trace.logItems "Build Output: "
    )
)

Core.Target.create "Release" (fun _ ->
    let releaseFilename = Path.Combine(releaseDir, String.Format("ES.Sojobo.v{0}.zip", releaseVersion))
    let buildDirectory = Path.Combine(buildDir, "ES.Sojobo")
    Directory.GetFiles(buildDirectory, "*.*", SearchOption.AllDirectories)
    |> Array.filter(fun file ->
        [".pdb"] 
        |> List.contains (Path.GetExtension(file).ToLowerInvariant())
        |> not
    )
    |> Fake.IO.Zip.zip buildDirectory releaseFilename
)

"Clean"        
    ==> "SetAssemblyInfo"
    ==> "Compile" 
    ==> "Release"
    
// Start build
Core.Target.runOrDefault "Release"