// design-time error in Rider, works during build.
open AdventOfCode2019
open System
open AdventOfCode2019.Day1Test

[<EntryPoint>]
let main argv =
    let tests = [
        ``calcFuel1 happy path``
        ``calcFuel1 fail``
    ]
    printfn "Test count: %d" tests.Length
    
    // if we get here, all tests passed (no assertions failed).  
    0