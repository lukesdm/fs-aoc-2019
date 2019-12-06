module AdventOfCode2019.Shared
open System

let toDigits (n: int) =
    // TODO: make this less ugly?
    string n |> Seq.map (fun c -> Int32.Parse (c.ToString ()))
    |> Seq.toArray
    

