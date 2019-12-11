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

type Points = Set<Point> // prob want to make this a Dictionary at some point

let addP ((x1, y1) : Point) ((x2, y2) : Point) =
    // Add 2 points
    (x1 + x2, y1 + y2)

let calcMarchingVector (p1: Point) (p2: Point) : Vector =
    // Calculate marching vector - the increment to check for occlusion between 2 points
    // Note reversal - march backwards from p2 towards p1
    let dx = (fst p1) - (fst p2)
    let dy = (snd p1) - (snd p2)
    let scale = calcHCF (Math.Abs(dx)) (Math.Abs(dy))
    (dx / scale, dy / scale)

//let isOccluded p1 p2 p3 = // won't know what p3 is!

let isOccluded p1 p2 (points: Points) =
    // Whether P2 is occluded from P1 by another point.
    // March from p2 towards p1, checking for existence of point
    let march = calcMarchingVector p1 p2
    let mutable curr = addP p2 march
    let mutable occluded = false
    while (curr <> p1 && not occluded) do
        occluded <- points.Contains(curr)
        curr <- addP curr march
    occluded

type Asteroid = Point
type Asteroids = Set<Asteroid>
let parse (input: string) : Asteroids =
    
    let lineFolder (x, y, asteroidsOnLine) char =
        ( x + 1,
          y,
          if (char = '#') then
                Asteroid (x, y) :: asteroidsOnLine
            else
                asteroidsOnLine
        )
    
    let gridFolder (y, asteroidsOnGrid) line =
        let (_, _, asteroidsOnLine) = line |> Seq.fold lineFolder (0, y, [])
        (y + 1, List.append asteroidsOnGrid asteroidsOnLine)
    
    let (_,asteroids) =
        input.Replace("\r", "")
            .Split("\n")
        |> Seq.fold gridFolder (0, [])
    
    Set.ofList asteroids
    

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
    
    let ``Occlusion check - hit`` () =
        // p3 occludes p2 from p1 i.e. p2 is occluded by p3
        let p1 = (3, 2)
        let p2 = (-1, -4)
        let p3 = (1, -1)
        let expected = true
        let actual = isOccluded p1 p2 (Set.ofList [p3])
        assert (expected = actual)
        
    let ``Occlusion check - miss`` () =
        // p2 is NOT occluded
        let p1 = (3, 2)
        let p2 = (-1, -4)
        let p3 = (1, 0)
        let expected = false
        let actual = isOccluded p1 p2 (Set.ofList [p3])
        assert (expected = actual)
    
    let ``Parse input - small`` () =
        let input = Day10_Data.small1
        let expected = Set.ofArray [| (0,0); (2,1); (1,2); (3,1) |]
        let actual = parse input
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
    ``Occlusion check - hit``()
    ``Occlusion check - miss``()
    ``Parse input - small``()
    //``example 1 - find best`` ()
    
    
