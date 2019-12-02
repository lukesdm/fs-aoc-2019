module AdventOfCode2019.Day2
open System.IO

// IntCode - see https://adventofcode.com/2019/day/2

type Program = int[]

let input : Program =
    (File.ReadAllText "day2-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse
    
let run program : Program =
    [|0|] // TODO: implement

//type OpCode =
//    | Add with InPosL: int, InPosR: int, OutPos: int // 1
//    | Multiply with InPosL: int, InPosR: int, OutPos: Int // 2
//    | Finish // 99