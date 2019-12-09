open System

open AdventOfCode2019

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
        
        Day3Test.``Can parse a wire description``
        Day3Test.``Can calculate distance``
        Day3Test.``Can read input``
    ]
    printfn "Test count: %d" tests.Length
    
    printfn "Day 1 part 1 result: %d" Day1.total1 // 3369286 verified as correct
    printfn "Day 1 part 2 result: %d" Day1.total2 // 5051054 verified as correct
    
    let endState = Day2.run Day2.initForPart1
    printfn "Day 2 part 1 result: %d" endState.[0] // 3765464 verified as correct
    
    let d2p2 = Day2.test
    printfn "Day 2 part 2 result: %d (output was %d)" (100 * d2p2.Noun + d2p2.Verb) d2p2.Output
    
    let d3p1 = Day3.test1
    printfn "Day 3 part 1 result: %d" d3p1 // 1211 verified as correct
    
    let _, d3p2 = Day3.test2
    printfn "Day 3 part 2 result: %d" d3p2
    
    // Day 4
    printf "%s" Day4.hello
    let d4p1 = Day4.calc1
    printfn "Day 4 part 1 result: %d" d4p1 // 1660 verified as correct

    let d4p2 = Day4.calc2
    printfn "Day 4 part 2 result: %d" d4p2 // 1135 verified as correct
    
    printf "%s" Day5.hello
    
    Day5.execute1() |> ignore
    
    Day5.runTests2()
    
    Day5.execute2() |> ignore
    
    Day8.runTests()
    Day8.execute()
    
    Day6.``tests - example``()
    Day6.execute()
    
    // if we get here, everything is OK - all tests passed (no assertions failed).  
    0