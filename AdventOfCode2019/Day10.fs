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
    let dx = (fst p2) - (fst p1)
    let dy = (snd p2) - (snd p1)
    let scale = calcHCF (Math.Abs(dx)) (Math.Abs(dy))
    (dx / scale, dy / scale)

//let isOccluded p1 p2 p3 = // won't know what p3 is!

/// Whether P2 is occluded from P1 by another point.
let isOccluded p1 p2 (points: Points) =
    // March from p1 towards p2, checking for existence of point
    let march = calcMarchingVector p1 p2
    let mutable curr = addP p1 march
    let mutable occluded = false
    while (curr <> p2 && not occluded) do
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
    

let countVisible (asteroids: Asteroids) (a1: Asteroid) =
    asteroids.Remove(a1) // don't count self
    |> Seq.filter (fun a2 -> isOccluded a1 a2 asteroids = false)
    |> Seq.length 

let findBest (asteroids: Asteroids) =
    // find asteroid with maximum number of non-occluded asteroids
    asteroids |> Seq.fold ( fun (best, bestCount) a1 ->
        let count = countVisible asteroids a1
        if count > bestCount then
            (a1, count) // new best
        else (best, bestCount)
        ) ((0, 0), 0) // TODO: clearer initial state

// PART 2...
// custom ordering based on rotation from vertical
type D = int // depth
type O1 = int // geometric order 1 - quadrant
type O2 = float // geometric order 2 - slope
//type O3 = int
type Key = D * O1 * O2// * O3
let calcOrderKey p0 pOther =
    // Calculates order based on rotational distance, using slope of marching vector.
    let mv = calcMarchingVector p0 pOther
    printfn "P0 = %A P1= %A MV = %A" p0 pOther mv

    let d = 1 // TODO: calculate
    
    let slope = float (snd mv) / float (fst mv)
    
    match (mv) with
    | (x, y) when x >= 0 && y <= 0 -> new Key (d, 0, slope) // 1st quadrant
    | (x, y) when x >= 0 && y > 0 -> new Key (d, 1, slope) // 2nd quadrant
    | (x, y) when x < 0 && y > 0 -> new Key (d, 2, slope) // 3rd quadrant
    | (x, y) when x < 0 && y <= 0 -> new Key (d, 3, slope) // 4th quadrant
    | _ -> failwith "unexpected input." 
      
let getOrderedPoints p0 points =
     let calcOrder = calcOrderKey p0
     let kvps = points |> Seq.map (fun p -> (calcOrder p), p)
     Map.ofSeq kvps 

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
        let expected = (-2, -3)
        let actual = calcMarchingVector p1 p2
        assert (expected = actual)
        
    let ``Marching vector - divisor 1`` () =
        let p1 = (1,1)
        let p2 = (8,2)
        let expected = (7, 1)
        let actual = calcMarchingVector p1 p2
        assert (expected = actual)
        
    let ``Marching vector - rectangular`` () =
        let p1 = (-1,-2)
        let p2 = (-1, 3)
        let expected = (0, 1)
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
        let expected = ((3,4), 8)  // Best = 8 from (3,4)
        let actual = example1 |> parse |> findBest
        assert (expected = actual)
        
    let ``example 2 - find best`` () =
        let expected = ((5,8), 33)  // Best = 33 from (5,8)
        let actual = example2 |> parse |> findBest
        assert (expected = actual)
        
    let ``example 3 - find best`` () =
        let expected = ((1,2), 35)  // Best = 35 from (1,2)
        let actual = example3 |> parse |> findBest
        assert (expected = actual)
        
    let ``example 4 - find best`` () =
        let expected = ((6,3), 41)  // Best = 41 from (6,3)
        let actual = example4 |> parse |> findBest
        assert (expected = actual)
        
    // Part 2 tests
    let ``can order based on rotation from vertical`` () =
        // clockwise rotation from grid-wise up (not cartesian up)
        
        let p0 = (4,5)  // centre point
        let points = [
            // Various quadrants and some along axes
            (9,7); (4,8); (3,6); (2,5)
            (4,3); (5,1); (6,4); (9,5)
            (2,4); (3,1); (6,7);
        ]
        let expected = [
            (4,3); (5,1); (6,4); (9,5)
            (9,7); (6,7); (4,8); (3,6)
            (2,5); (2,4); (3,1)
        ]
        let pointMap = getOrderedPoints p0 points
        let actual = pointMap |> Map.toList |> List.map (fun (_, point) -> point )
        
        assert (expected = actual)
        
        // TODO: Next test - D
         
    
        
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
    ``example 1 - find best`` ()
    ``example 2 - find best`` ()
    ``example 3 - find best`` ()
    ``example 4 - find best`` ()
    ``can order based on rotation from vertical``()
    
let execute () =
    let result = problemInput |> parse |> findBest
    printfn "Day 10 part 1 result = %A" result // 309 verified correct (pos 37, 25)
