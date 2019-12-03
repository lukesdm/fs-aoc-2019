module AdventOfCode2019.Day3
open System.Text.RegularExpressions

// Intersecting Wires
// See https://adventofcode.com/2019/day/3

// 2 wires. Inputs of the form R8,U5,L5,D3,... (starting at origin)
// Find intersection closest the origin (by manhattan distance)
type Point = {x: int; y: int}
type WireSegment = {p1: Point; p2: Point}

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
    