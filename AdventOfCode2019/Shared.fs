module AdventOfCode2019.Shared
open System

let intToDigits (n: int) =
    // TODO: make this less ugly?
    string n |> Seq.map (fun c -> Int32.Parse (c.ToString ()))
    |> Seq.toArray
    
let int64ToDigits (n: int64) =
    // TODO: make this less ugly?
    string n |> Seq.map (fun c -> Int64.Parse (c.ToString ()))
    |> Seq.toArray
    
let stringToDigits (s: string) =
    s |> Seq.map (fun c -> Int32.Parse (c.ToString ()))
    |> Seq.toArray

