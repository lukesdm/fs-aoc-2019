module AdventOfCode2019.Day10
open System;
open AdventOfCode2019
open AdventOfCode2019.Day10_Data

// Support functions
// Highest common factor aka lowest common divisor
let rec calcHCF n1 n2 =
    if n2 = 0 then n1
    else calcHCF n2 (n1 % n2)
    
type X = int
type Y = int
type Point = X * Y
type Vector = X * Y
let calcMarchingVector (p1: Point) (p2: Point) : Vector =
    // Calculate marching vector - the increment to check for occlusion between 2 points
    // Note reversal - march backwards from p2 towards p1
    let dx = (fst p1) - (fst p2)
    let dy = (snd p1) - (snd p2)
    let scale = calcHCF (Math.Abs(dx)) (Math.Abs(dy))
    (dx / scale, dy / scale)
        
// type Grid = ...
let parse input =
    [|0|]

let findBest grid =
    (0, (0,0))

let runTests () =
    let ``HCF calc - happy path`` () =
        let expected = 6
        let actual = calcHCF 48 18
        assert (expected = actual)
        
    let ``HCF calc - rev`` () =
        let expected = 6
        let actual = calcHCF 18 48
        assert (expected = actual)
        
    let ``HCF calc - with prime`` () =
        let expected = 1
        let actual = calcHCF 53 18
        assert (expected = actual)
        
    let ``Marching vector - divisor 2`` () =
        let p1 = (3,2)
        let p2 = (-1, -4)
        let expected = (2, 3) // marches backwards from p2
        let actual = calcMarchingVector p1 p2
        assert (expected = actual)
        
    let ``Marching vector - divisor 1`` () =
        let p1 = (1,1)
        let p2 = (8,2)
        let expected = (-7, -1)
        let actual = calcMarchingVector p1 p2
        assert (expected = actual)
        
    let ``Marching vector - rectangular`` () =
        let p1 = (-1,-2)
        let p2 = (-1, 3)
        let expected = (0,-1)
        let actual = calcMarchingVector p1 p2
        assert (expected = actual)
    
    let ``example 1 - find best`` () =
        let expected = (8, (3,4))  // Best = 8 from (3,4)
        let actual = example1 |> parse |> findBest
        assert (expected = actual)
        
    ``HCF calc - happy path``()
    ``HCF calc - rev``()
    ``HCF calc - with prime``()
    //``HCF calc - with negative``() // don't support -ve
    ``Marching vector - divisor 1``()
    ``Marching vector - divisor 2``()
    ``Marching vector - rectangular``()
    //``example 1 - find best`` ()
    
    
