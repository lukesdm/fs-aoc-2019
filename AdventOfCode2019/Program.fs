// design-time error in Rider, works during build.
open AdventOfCode2019.Day1
open System
open AdventOfCode2019.Day1Test

[<EntryPoint>]
let main argv =
    let tests = [
        ``calcFuel1 happy path``
        ``calcFuel2 small``
        ``calcFuel2 regular``
        ``calcFuel2 regular2``
    ]
    printfn "Test count: %d" tests.Length
    
    printfn "Day 1 part 1 result: %d" total1
    printfn "Day 1 part 2 result: %d" total2
    
    // if we get here, everything is OK - all tests passed (no assertions failed).  
    0