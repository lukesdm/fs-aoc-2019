module AdventOfCode2019.Day3Test
open AdventOfCode2019.Day3

// Example 1.
// Wire 1: R8,U5,L5,D3
// Wire 2: U7,R6,D4,L4
let ``Can parse a wire description`` =
    let wireDescription = "R8,U5,L5,D3"
    let expectedWireSegments = [|
        { p1 = {x = 0; y= 0}; p2 = {x = 8; y= 0} }
        { p1 = {x = 8; y= 0}; p2 = {x = 8; y= 5} }
        { p1 = {x = 8; y= 5}; p2 = {x = 3; y= 5} }
        { p1 = {x = 3; y= 5}; p2 = {x = 3; y= 2} }
    |]
    let actualWireSegments = parseWireDescription wireDescription
    assert (actualWireSegments = expectedWireSegments)
    

//let ``can parse input`` = 
//    let input = [|"R8,U5,L5,D3"; "U7,R6,D4,L4" |]
//    let ab = input |> Array.map (fun (s: string) -> s.Split ",") 