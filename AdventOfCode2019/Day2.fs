module AdventOfCode2019.Day2
open System.IO

// IntCode - see https://adventofcode.com/2019/day/2

// Program is a mutable fixed length collection of integers
type Program = int[]

let input : Program =
    (File.ReadAllText "day2-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse
    
type OpArgs = { In1Pos : int; In2Pos : int ; OutPos : int }
type Operation =
    | Halt
    | Add of OpArgs
    | Multiply of OpArgs
    | Undefined

let parse (block: int[]) : Operation =
    match block with
    | b when b.[0] = 99 -> Halt
    | [|1; inPos1; inPos2; outPos|] -> Add { In1Pos = inPos1; In2Pos = inPos2; OutPos = outPos }
    | [|2; inPos1; inPos2; outPos|] -> Multiply { In1Pos = inPos1; In2Pos = inPos2; OutPos = outPos }
    | _ -> failwith "Unexpected input"
    // TODO: Tidy up

let eval (program: Program) operation =
    match operation with
    | Add a -> program.[a.OutPos] <- program.[a.In1Pos] + program.[a.In2Pos]
    | _ -> ignore()
    
let run (program : Program) =
    let mutable pc = 0
    let mutable operation : Operation = Undefined 
    while not (operation = Halt) do
        assert (pc < program.Length)
        
        let block = program.[pc..System.Math.Min (pc+3, program.Length-1)]
        operation <- parse block
        
        eval program operation
        
        // each operation uses 4 blocks, apart from Halt, the final one
        pc <- pc + 4 
        
    program

//type OpCode =
//    | Add with InPosL: int, InPosR: int, OutPos: int // 1
//    | Multiply with InPosL: int, InPosR: int, OutPos: Int // 2
//    | Finish // 99