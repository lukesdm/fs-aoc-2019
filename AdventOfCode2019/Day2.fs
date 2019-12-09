module AdventOfCode2019.Day2
open System.IO

// IntCode computer - see https://adventofcode.com/2019/day/2

// Program is a mutable fixed length collection of integers
type Program = int[]

let input : Program =
    (File.ReadAllText "Auxi\day2-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse

let initializeProgram noun verb =
    let result = Array.copy input
    result.[1] <- noun
    result.[2] <- verb
    result

// Part 1: "before running the program, replace position 1 with the value 12 and replace position 2 with the value 2"
let initForPart1 = initializeProgram 12 2
    
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

let eval (program: Program) instruction =
    match instruction with
    | Add op -> program.[op.OutAddress] <- program.[op.In1Address] + program.[op.In2Address]
    | Multiply op -> program.[op.OutAddress] <- program.[op.In1Address] * program.[op.In2Address]
    | _ -> ignore()
    
let run (program : Program) =
    let mutable finished = false
    
    Seq.chunkBySize 4 program
    |> Seq.iter (
        fun chunk ->
            if not finished then
                let instruction = parse chunk
                eval program instruction
                finished <- (instruction = Halt)
            // NOP for remaining chunks
        )
    program

//let mutable pc = 0
//let mutable instruction : Instruction = Undefined
// Original iterative implementation: 
//    while not (instruction = Halt) do
//        assert (pc < program.Length)
//        
//        let block = program.[pc..System.Math.Min (pc+3, program.Length-1)]
//        instruction <- parse block
//        
//        eval program instruction
//        
//        // each operation uses 4 blocks, apart from Halt, the final one
//        pc <- pc + 4 
//    program


// PART 2:
// Once the program has halted, its output is available at address 0
// Find the input noun and verb that cause the program to produce the output 19690720
// Approach: Manual search was sufficient.
type Candidate = { Output: int; Noun: int; Verb: int }
let test =
    // Some sample results
    // 0, 0 -> 779478
    // 12, 2 -> 3765464
    // 50, 2 -> 13221080
    // 75, 2 -> 19441880
    // 76, 2 -> 19690712
    // n, v -> 19690720
    // 77, 2 -> 19939544
    // 100, 1 -> 25662679
    // 100, 2 -> 25662680
    
    let noun = 76
    let verb = 10
    let endState =
        initializeProgram noun verb
        |> run
    
    { Output = endState.[0]; Noun = noun; Verb = verb }