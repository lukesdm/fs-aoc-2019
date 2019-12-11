module AdventOfCode2019.Day10
open AdventOfCode2019
open AdventOfCode2019.Day10_Data

// type Grid = ...
let parse input =
    [|0|]

let findBest grid =
    (0, (0,0))

let runTests () =
    let ``example 1 - find best`` () =
        let expected = (8, (3,4))  // Best = 8 from (3,4)
        let actual = example1 |> parse |> findBest
        assert (expected = actual)
        
    ``example 1 - find best`` ()
    
