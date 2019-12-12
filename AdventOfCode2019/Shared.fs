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

type ImmArray(initialMemory: int[]) =
    member this.Item
        with get (index: int64) =
            initialMemory.GetValue(index) :?> int64
//        and set (index: int64) (value: int64) =
//            initialMemory.SetValue(value, index)
    