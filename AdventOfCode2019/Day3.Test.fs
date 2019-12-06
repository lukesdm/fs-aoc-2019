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
    let actualWireSegments = parseWireDescription1 wireDescription
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
        let actualDist = calcDistance1 o p
        assert (expectedDist = actualDist)
        )
    
let ``Can read input`` = 
    let wires = readInput1
    let wireA, wireB = wires.[0], wires.[1] 
    assert (wireA.Length > 0)
    assert (wireB.Length > 0)
    
let ``Can check intersection - yes`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 1}; p2 = {x = 2; y = 1}}
    let expected = true
    let actual = intersects a b
    assert (expected = actual)
    
let ``Can check intersection - yes - rev segments`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 1}; p2 = {x = 2; y = 1}}
    let expected = true
    let actual = intersects b a
    assert (expected = actual)
    
let ``Can check intersection - yes - rev points`` =
    let a = { p2 = {x = 1; y = 0}; p1 = {x = 1; y = 2}}
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
    
let ``Can check intersection - no - rev segments`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 3}; p2 = {x = 2; y = 3}}
    let expected = false
    let actual = intersects b a
    assert (expected = actual)
    
let ``Can check intersection - no - rev points`` =
    let a = { p2 = {x = 1; y = 0}; p1 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 3}; p2 = {x = 2; y = 3}}
    let expected = false
    let actual = intersects a b
    assert (expected = actual)
    
    // TODO: Test with negatives?
    
let ``calc intersection 1`` =
    let a = { p1 = {x = 1; y = 0}; p2 = {x = 1; y = 2}}
    let b = { p1 = {x = 0; y = 1}; p2 = {x = 2; y = 1}}
    let expected = { x = 1; y = 1 }
    let actual = calcIntersection a b
    assert (expected = actual)

// Example 1.
// Wire 1: R8,U5,L5,D3
// Wire 2: U7,R6,D4,L4
// 2 intersections - (3,3) and (6,5)
let ``Can calculate intersections`` =
    let wireA = parseWireDescription1 "R8,U5,L5,D3"
    let wireB = parseWireDescription1 "U7,R6,D4,L4"
    let expectedIntesections = Set [
        {x = 3; y = 3};
        {x = 6; y = 5}
    ]
    let actualIntersections = calcIntersections wireA wireB
    assert (expectedIntesections = actualIntersections)

let ``Can find distance to nearest intersection`` =
    let wireA = parseWireDescription1 "R8,U5,L5,D3"
    let wireB = parseWireDescription1 "U7,R6,D4,L4"
    let expected = 6
    let actual = findClosestIntersection1 wireA wireB
    assert (expected = actual)

//Here are a few more examples:
//R75,D30,R83,U83,L12,D49,R71,U7,L72
//U62,R66,U55,R34,D71,R55,D58,R83 = distance 159
let ``Can find distance to nearest intersection 2`` =
    let wireA = parseWireDescription1 "R75,D30,R83,U83,L12,D49,R71,U7,L72"
    let wireB = parseWireDescription1 "U62,R66,U55,R34,D71,R55,D58,R83"
    let expected = 159
    let actual = findClosestIntersection1 wireA wireB
    assert (expected = actual)
    
//R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
//U98,R91,D20,R16,D67,R40,U7,R15,U6,R7 = distance 135
let ``Can find distance to nearest intersection 3`` =
    let wireA = parseWireDescription1 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    let wireB = parseWireDescription1 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
    let expected = 135
    let actual = findClosestIntersection1 wireA wireB
    assert (expected = actual)
    
// PART 2...

// Self-intersecting.
let siTestWire1Desc = "R3,U2,L1,D4"
let siTestWire2Desc = "D1,R4,U2,L3"

let ``Part 2: can parse self intersecting, single`` =
    let expected = new Wire2 ([
        { p1 = {x=0; y=0}; p2 = {x=3; y=0}; plStart = 0; plEnd = 3 ; split = false }
        { p1 = {x=3; y=0}; p2 = {x=3; y=2}; plStart = 3; plEnd = 5 ; split = false }
        { p1 = {x=3; y=2}; p2 = {x=2; y=2}; plStart = 5; plEnd = 6 ; split = false }
        { p1 = {x=2; y=2}; p2 = {x=2; y=0}; plStart = 6; plEnd = 2 ; split = true }
        { p1 = {x=2; y=0}; p2 = {x=2; y= -2}; plStart = 2; plEnd = 4 ; split = true }
    ])
    let actual = parseWireDescription2 siTestWire1Desc None
    assert (expected.ToArray() = actual.ToArray()) 
    
let ``Part 2: calc wire intersections, simple`` =
    let wireA = parseWireDescription2 siTestWire1Desc None
    let wireB = parseWireDescription2 siTestWire2Desc None
    let expected = 6 // (-1, 2) i think?
    let _, actual = findClosestIntersection2 wireA wireB
    assert (expected = actual)
    
let ``Part 2: wires cross after self intersection `` =
    let wireA = parseWireDescription2 siTestWire1Desc None
    let wireB = parseWireDescription2 "D3,R4,U4,L3" None // c.f. "D1,R4,U2,L3"
    let expected = 16
    let intersection, actual = findClosestIntersection2 wireA wireB
    assert (expected = actual)



let ``Part 2: example 1`` =
    let wireA = parseWireDescription2 "R8,U5,L5,D3" None
    let wireB = parseWireDescription2 "U7,R6,D4,L4" None
    let expected = 30
    let _, actual = findClosestIntersection2 wireA wireB
    assert (expected = actual)

let ``Part 2: example 2`` =
    let wireA = parseWireDescription2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" None
    let wireB = parseWireDescription2 "U62,R66,U55,R34,D71,R55,D58,R83" None
    let expected = 610
    let _, actual = findClosestIntersection2 wireA wireB
    assert (expected = actual)

let ``Part 2: example 3`` =
    let wireA = parseWireDescription2 "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" None
    let wireB = parseWireDescription2 "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7" None
    let expected = 410
    let _, actual = findClosestIntersection2 wireA wireB
    assert (expected = actual)