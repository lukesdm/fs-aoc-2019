open System

open AdventOfCode2019
open AdventOfCode2019.Day1
open AdventOfCode2019.Day1Test
open AdventOfCode2019.Day2

[<EntryPoint>]
let main argv =
    let tests = [
        Day1Test.``calcFuel1 happy path``
        Day1Test.``calcFuel2 small``
        Day1Test.``calcFuel2 regular``
        Day1Test.``calcFuel2 regular2``
        
        Day2Test.``Add test``
    ]
    printfn "Test count: %d" tests.Length
    
    printfn "Day 1 part 1 result: %d" total1
    printfn "Day 1 part 2 result: %d" total2
    
    // if we get here, everything is OK - all tests passed (no assertions failed).  
    0