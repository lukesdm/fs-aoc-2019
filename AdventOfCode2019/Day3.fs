module AdventOfCode2019.Day3
open System
open System.IO
open System.Text.RegularExpressions

// Intersecting Wires
// See https://adventofcode.com/2019/day/3

// 2 wires. Inputs of the form R8,U5,L5,D3,... (starting at origin)
// Find intersection closest the origin (by manhattan distance)
type Point = {x: int; y: int}
type WireSegment = {p1: Point; p2: Point}
type Wire = WireSegment[]

let parseWireDescription (input: string) =
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
let calcDistance p1 p2 =
    Math.Abs(p2.x - p1.x) + Math.Abs(p2.y - p1.y)
    
// n1 <= x <= n2
let isBetween x n1 n2 =
    (n1 <= x && x <= n2)
    || (n2 <= x && x <= n1)

let intersects (segA: WireSegment) (segB: WireSegment) =
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

 
let calcIntersection (segA: WireSegment) (segB: WireSegment) =
    // Quick and dirty - probably won't handle overlaps well.
    // Assume the 'has intersection' check has been performed...
    // Uses fact that there are only horizontal and vertical lines,
    // i.e. x and y are constant throughout line segment
    let intersection =
        if segA.p1.y = segA.p2.y
        then { x = segB.p1.x; y = segA.p1.y }
        else { x = segA.p1.x; y = segB.p1.y }
    intersection
    
let calcIntersections (wireA: Wire) (wireB: Wire): Set<Point> =
    let mutable intersections = Set.empty
    //intersections <- intersections.Add {x = 0; y = 0}
    wireB |> Array.iter (fun segB ->
        wireA |> Array.iter (fun segA ->
            if intersects segA segB then
                intersections <- intersections.Add
                                 <| calcIntersection segA segB
        )
    )
    intersections.Remove {x = 0; y = 0}

let findClosestIntersection (wireA : Wire) (wireB : Wire) =
    let closest =
        calcIntersections wireA wireB
        |> Set.map (calcDistance {x = 0; y = 0})
        |> Set.minElement
    closest
    
let readInput =
    File.ReadAllLines "day3-input.txt"
    |> Array.map parseWireDescription