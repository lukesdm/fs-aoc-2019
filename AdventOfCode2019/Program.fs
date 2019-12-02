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
        Day2Test.``Multiply test 1``
        Day2Test.``Multiply test 2``
        Day2Test.``Combined test``
    ]
    printfn "Test count: %d" tests.Length
    
    printfn "Day 1 part 1 result: %d" Day1.total1 // 3369286 verified as correct
    printfn "Day 1 part 2 result: %d" Day1.total2 // 5051054 verified as correct
    
    let x = Day2.run Day2.restoredProgram
    printfn "Day 2 part 1 result: %d" x.[0] // 3765464 verified as correct
    
    // if we get here, everything is OK - all tests passed (no assertions failed).  
    0