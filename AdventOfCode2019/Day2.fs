module AdventOfCode2019.Day2
open System.IO

// IntCode - see https://adventofcode.com/2019/day/2

// Program is a mutable fixed length collection of integers
type Program = int[]

let input : Program =
    (File.ReadAllText "day2-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse

// "before running the program, replace position 1 with the value 12 and replace position 2 with the value 2"
let restoredProgram =
    let result = Array.copy input
    result.[1] <- 12
    result.[2] <- 2
    result
    
type OpParams = { In1Address : int; In2Address : int ; OutAddress : int }
type Instruction =
    | Halt
    | Add of OpParams
    | Multiply of OpParams
    | Undefined

let parse (block: int[]) : Instruction =
    match block with
    | b when b.[0] = 99 -> Halt
    | [| opCode; inPos1; inPos2; outPos |] ->
        let opParams = { In1Address = inPos1; In2Address = inPos2; OutAddress = outPos }
        match opCode with
        | 1 -> Add opParams
        | 2 -> Multiply opParams
        | _ -> failwith "Unexpected opCode"
    | _ -> failwith "Bad input format"

let eval (program: Program) operation =
    match operation with
    | Add op -> program.[op.OutAddress] <- program.[op.In1Address] + program.[op.In2Address]
    | Multiply op -> program.[op.OutAddress] <- program.[op.In1Address] * program.[op.In2Address]
    | _ -> ignore()
    
let run (program : Program) =
    let mutable pc = 0
    let mutable operation : Instruction = Undefined 
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