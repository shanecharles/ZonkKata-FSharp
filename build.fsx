#r "packages/FAKE/tools/fakelib.dll"

open Fake

let buildDir = "./build/"
let testDir = "./tests/"

Target "Clean" (fun _ ->
    CleanDirs [buildDir; testDir]
)

Target "BuildApp" (fun _ ->
    !! "src/app/**/*.fsproj"
      |> MSBuildRelease buildDir "Build"
      |> Log "AppBuild-Output: "
)

Target "BuildTest" (fun _ ->
    !! "src/tests/**/*.fsproj"
      |> MSBuildDebug testDir "Build"
      |> Log "TestBuild-Output: "
)

Target "Default" (fun _ ->
    trace "Build from fake"
)

"Clean"
  ==> "BuildApp"
  ==> "BuildTest"
  ==> "Default"

RunTargetOrDefault "Default"
