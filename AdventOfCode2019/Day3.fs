module AdventOfCode2019.Day3
open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

// Intersecting Wires
// See https://adventofcode.com/2019/day/3

// 2 wires. Inputs of the form R8,U5,L5,D3,... (starting at origin)
// Find intersection closest the origin (by manhattan distance)
type Point = {x: int; y: int}
type WireSegment1 = {p1: Point; p2: Point}
type Wire1 = WireSegment1[]

let parseWireDescription1 (input: string) =
    let regex = new Regex "(U|D|L|R)(\d+)" // grp1 = dir, grp2 = amount
    let mutable cursor: Point = {x=0; y=0}
    let tokens = input.Split(",")
    let segments = Array.zeroCreate tokens.Length 
    tokens |> Array.iteri (fun i token ->
        let rmatch = regex.Match token 
        let (dir, amt) = rmatch.Groups.[1].Value, int rmatch.Groups.[2].Value
        let segment =
            match dir with
            | "U" -> { p1 = cursor; p2 = { cursor with y = (cursor.y + amt)} }
            | "D" -> { p1 = cursor; p2 = { cursor with y = (cursor.y - amt)} }
            | "R" -> { p1 = cursor; p2 = { cursor with x = (cursor.x + amt)} }
            | "L" -> { p1 = cursor; p2 = { cursor with x = (cursor.x - amt)} }
            | _ -> failwith "Unexpected input"
        segments.[i] <- segment
        cursor <- segment.p2
        )
    segments
    
// Manhattan distance
let calcDistance1 p1 p2 =
    Math.Abs(p2.x - p1.x) + Math.Abs(p2.y - p1.y)
    
// n1 <= x <= n2
let isBetween x n1 n2 =
    (n1 <= x && x <= n2)
    || (n2 <= x && x <= n1)

let intersects (segA: WireSegment1) (segB: WireSegment1) =
    let res =
        (isBetween segB.p1.x segA.p1.x segA.p2.x
        && isBetween segB.p2.x segA.p1.x segA.p2.x
        && isBetween segA.p1.y segB.p1.y segB.p2.y
        && isBetween segA.p2.y segB.p1.y segB.p2.y)
        ||
        (isBetween segB.p1.y segA.p1.y segA.p2.y
        && isBetween segB.p2.y segA.p1.y segA.p2.y
        && isBetween segA.p1.x segB.p1.x segB.p2.x
        && isBetween segA.p2.x segB.p1.x segB.p2.x)
    res

 
let calcIntersection (segA: WireSegment1) (segB: WireSegment1) =
    // Quick and dirty - probably won't handle overlaps well.
    // Assume the 'has intersection' check has been performed...
    // Uses fact that there are only horizontal and vertical lines,
    // i.e. x and y are constant throughout line segment
    let intersection =
        if segA.p1.y = segA.p2.y
        then { x = segB.p1.x; y = segA.p1.y }
        else { x = segA.p1.x; y = segB.p1.y }
    intersection
    
let calcIntersections (wireA: Wire1) (wireB: Wire1): Set<Point> =
    let mutable intersections = Set.empty
    wireB |> Array.iter (fun segB ->
        wireA |> Array.iter (fun segA ->
            if intersects segA segB then
                intersections <- intersections.Add
                                 <| calcIntersection segA segB
        )
    )
    intersections.Remove {x = 0; y = 0}

let findClosestIntersection1 (wireA : Wire1) (wireB : Wire1) =
    // It works for the sample input but it's pretty inefficient.
    // Simple way to improve it - pass a callback into the parser for wireB
    // and do the intersection calc and distance, keeping track of the closest.
    let closest =
        calcIntersections wireA wireB
        |> Set.map (calcDistance1 {x = 0; y = 0})
        |> Set.minElement
    closest
    
let readInput1 =
    File.ReadAllLines "Auxi\day3-input.txt"
    |> Array.map parseWireDescription1
    
let test1 =
    let wires = readInput1
    findClosestIntersection1 wires.[0] wires.[1]
    
// PART 2...
type Orientation =
    | Horizontal
    | Vertical

type WireSegment2 =
    { p1: Point;
      p2: Point;
      split: bool // for debugging 
      plStart: int
      plEnd: int } // path length at end of segment
    member this.orientation
        with get() =
            if this.p1.x = this.p2.x then Vertical else Horizontal
    member this.length
        with get() =
            match this.orientation with
            | Horizontal -> Math.Abs (this.p2.x - this.p1.x)
            | Vertical -> Math.Abs (this.p2.y - this.p1.y)
                           
type Wire2 = ResizeArray<WireSegment2> // supports additional entries due to segment splitting
type Intersection = { p: Point; segmentA: WireSegment2; segmentB: WireSegment2 }

let calcIntersection2 (segA: WireSegment2) (segB: WireSegment2) = 
    let intersects =
            (isBetween segB.p1.x segA.p1.x segA.p2.x
            && isBetween segB.p2.x segA.p1.x segA.p2.x
            && isBetween segA.p1.y segB.p1.y segB.p2.y
            && isBetween segA.p2.y segB.p1.y segB.p2.y)
            ||
            (isBetween segB.p1.y segA.p1.y segA.p2.y
            && isBetween segB.p2.y segA.p1.y segA.p2.y
            && isBetween segA.p1.x segB.p1.x segB.p2.x
            && isBetween segA.p2.x segB.p1.x segB.p2.x)

    if intersects then
        if segA.p1.y = segA.p2.y
        then Some { p = {x = segB.p1.x; y = segA.p1.y}; segmentA = segA; segmentB = segB }
        else Some { p = {x = segA.p1.x; y = segB.p1.y}; segmentA = segA; segmentB = segB }
    else None

let rec findIntersections (wireSegs: list<WireSegment2>) (newSeg: WireSegment2) (intersections) =
    if not wireSegs.IsEmpty then
        // TODO: potential issue with using newSeg here - will have inaccurate pathLength on subsequent calls - to fix would need to pass in result of split. Shouldn't be an issue if that seg is not used in calcs
        let intersection = calcIntersection2 newSeg wireSegs.Head
        if intersection.IsSome then
            findIntersections wireSegs.Tail newSeg (intersection.Value :: intersections)
        else
            findIntersections wireSegs.Tail newSeg intersections
    else
        intersections

let checkSelfIntersection (wire: Wire2) (newSegment: WireSegment2) =
    // Note: need to consider multiple intersections, so traverse all segments
    
    // hacky way to remove last segment so it's not counted as an intersection 
    let input =
        if wire.Count > 0 then
            ((Seq.toList wire |> List.rev).Tail |> List.rev)
        else
            list.Empty
    
    findIntersections input newSegment List.empty

let dist p1 p2 =
    if p1.x = p2.x then Math.Abs (p2.y - p1.y)
    elif p1.y = p2.y then Math.Abs (p1.x - p2.x)
    else failwith "expected a horizontal or vertical distance"
    
let calcNewPathLength (segA: WireSegment2) (intersection: Intersection)  =
    // when segA is intercepted by segB, take segBs pathlength (which will be shorter)
    //  minus the distance along the segment from the intersection  
    let segB = intersection.segmentB
    let distFromEnd = dist segB.p2 intersection.p
    segB.plEnd - distFromEnd  
    
let split origSegment intersections =
    // Split segment according to intersections, assigning new pathlength
    let mutable currSeg = origSegment
    let mutable segs: list<WireSegment2> = list.Empty
    
    intersections |> Seq.iter ( fun intersection ->
        let newPathLength = calcNewPathLength currSeg intersection 
        let newSeg = { p1 = currSeg.p1; p2 = intersection.p; plStart = currSeg.plStart; plEnd = newPathLength; split = true  }
        currSeg <- newSeg
        segs <- newSeg :: segs )
    let finalSeg =
        { p1 = currSeg.p2;
          p2 = origSegment.p2;
          split = true
          plStart = currSeg.plEnd
          plEnd = currSeg.plEnd + (dist currSeg.p2 origSegment.p2) }
    segs <- finalSeg :: segs 
    List.rev segs

type SegsAddedCallback = list<WireSegment2>->unit
let parseWireDescription2 (input: string) (segsAddedCallback: option<SegsAddedCallback>) =
    let regex = new Regex "(U|D|L|R)(\d+)" // grp1 = dir, grp2 = amount
    let mutable prevSeg : WireSegment2 = {p1 = {x=0; y=0}; p2={x=0; y=0;}; plStart = 0; plEnd = 0;  split = false}
    let tokens = input.Split(",")
    let segments = new Wire2 (tokens.Length) // we know it's at least as big as this
    tokens |> Array.iter (fun token ->
        let rmatch = regex.Match token 
        let (dir, length) = rmatch.Groups.[1].Value, int rmatch.Groups.[2].Value
        let plEnd = prevSeg.plEnd + length
        
        let newP2 =
            match dir with
            | "U" -> { prevSeg.p2 with y = (prevSeg.p2.y + length) }
            | "D" -> { prevSeg.p2 with y = (prevSeg.p2.y - length) }
            | "R" -> { prevSeg.p2 with x = (prevSeg.p2.x + length) }
            | "L" -> { prevSeg.p2 with x = (prevSeg.p2.x - length) }
            | _ -> failwith "Unexpected input"
        
        let newSegment = { p1 = prevSeg.p2; p2 = newP2; plStart = prevSeg.plEnd; plEnd = plEnd; split = false }
        
        let selfIntersections = checkSelfIntersection segments newSegment 
        
        let newSegs =
            if selfIntersections.IsEmpty then
                [newSegment]
            else
                split newSegment selfIntersections
        
        segments.AddRange newSegs
        if segsAddedCallback.IsSome then
           segsAddedCallback.Value newSegs
        prevSeg <- (List.rev newSegs).Head
        )
    segments
    // TODO: another potential issue - should intersection split *both* segments? (and recalculate pathLength)
    
let calcIntersections2 (wireA: Wire2) (wireB: Wire2) =
    let intersections = new ResizeArray<Intersection>()
    wireA |> Seq.iter (fun segA ->
        intersections.AddRange (findIntersections (Seq.toList wireB) segA list.Empty)
        )
    intersections.RemoveAt(0) // (0,0)
    intersections

// sum of wire pathlength to intersection
let calcDistance2 (intersection: Intersection) =
    let pathLengthForWire seg p =
        seg.plStart + (dist seg.p1 p)
    
    let plA = pathLengthForWire intersection.segmentA intersection.p
    let plB = pathLengthForWire intersection.segmentB intersection.p
    plA + plB
    
let findClosestIntersection2 (wireA: Wire2) (wireB: Wire2) =
    let closest =
        calcIntersections2 wireA wireB
        |> Seq.minBy calcDistance2
    (closest, calcDistance2 closest)
    
// for viz
 
// x,y coords of lower left and top right corners 
type Rect = ((int*int) * (int*int))

let getBounds (segments: seq<WireSegment2>) : Rect =
    let initial = ((Int32.MaxValue,Int32.MaxValue),(Int32.MinValue,Int32.MinValue))
    let boundingRect =
        (initial, segments) ||> Seq.fold (
            fun ((minX, minY), (maxX, maxY)) seg ->
                (Seq.min [|seg.p1.x; seg.p2.x; minX|], Seq.min [|seg.p1.y; seg.p2.y; minY|]),
                (Seq.max [|seg.p1.x; seg.p2.x; maxX|], Seq.max [|seg.p1.y; seg.p2.y; maxY|])
        )
    boundingRect

// for viz - gets scale and offsets required to fit wire onto canvas
let getTransform srcRect destRect =
    // return a scale and a shift
    // o = origin, c = top-right corner
    let (ox1, oy1), (cx1, cy1) = srcRect
    let (ox2, oy2), (cx2, cy2) = destRect
    let w1, h1 = cx1-ox1, cy1-oy1
    let w2, h2 = cx2-ox2, cy2-oy2
    let scaleX, scaleY = float w2 / w1, float h2 / h1
    let offsetX, offsetY = float ox2-ox1, float oy2-oy1
    (scaleX, scaleY), (offsetX, offsetY)
    
let test2 =
    let callback: option<SegsAddedCallback> = None
//        Some (fun segs -> printfn "Seg count: %d" (List.length segs))
    
    let wires =
        File.ReadAllLines "Auxi\day3-input.txt"
        |> Array.map (fun desc -> parseWireDescription2 desc callback) 
     
    let bounds = getBounds (Seq.append (wires.[0] :> seq<WireSegment2>) (wires.[1] :> seq<WireSegment2>))
    
    findClosestIntersection2 wires.[0] wires.[1] 