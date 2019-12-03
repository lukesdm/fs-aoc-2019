module AdventOfCode2019.Day3Test
open AdventOfCode2019.Day3

// Example 1.
// Wire 1: R8,U5,L5,D3
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

let ``Can calculate distance`` =
    let o = {x = 0; y = 0}
    [|
        // input point, expected dist
        ({x = 3; y = 3;}, 6)
        ({x = 3; y = 4;}, 7)
        ({x = -3; y = 2;}, 5)
        ({x = 1; y = -8;}, 9)
    |]
    |> Array.iter ( fun t ->
        let (p, expectedDist) = t
        let actualDist = calcDistance o p
        assert (expectedDist = actualDist)
        )
    
let ``Can read input`` = 
    let wires = readInput
    let wireA, wireB = wires.[0], wires.[1] 
    assert (wireA.Length > 0)
    assert (wireB.Length > 0)
    
let ``Can check intersection - yes`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 1}; p2 = {x = 2; y = 1}}
    let expected = true
    let actual = intersects a b
    assert (expected = actual)
    
let ``Can check intersection - no`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 3}; p2 = {x = 2; y = 3}}
    let expected = false
    let actual = intersects a b
    assert (expected = actual)
    
    // TODO: Test with P1 and P2 in different orders!

// Example 1.
// Wire 1: R8,U5,L5,D3
// Wire 2: U7,R6,D4,L4
// 2 intersections - (3,3) and (6,5)
let ``Can calculate intersections`` =
    let wireA = parseWireDescription "R8,U5,L5,D3"
    let wireB = parseWireDescription "U7,R6,D4,L4"
    let expectedIntesections = Set [
        {x = 3; y = 3};
        {x = 6; y = 5}
    ]
    let actualIntersections = calcIntersections wireA wireB
    assert (expectedIntesections = actualIntersections)

// TODO: Other examples