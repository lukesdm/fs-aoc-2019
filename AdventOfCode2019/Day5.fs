module AdventOfCode2019.Day5
open AdventOfCode2019
open Shared
open System.IO

let hello = "just need a reference to trigger the code in here."

// Program/memory
type Program = int[]
    
//type ArithParams = { In1Address : int; In2Address : int ; OutAddress : int }
    
type Mode =
    | Position = 0
    | Immediate = 1
type Value = int
type Param = Mode * Value
type Params =
    | Zilch // Zero (avoid naming conflict)
    | One of Param
    | Three of Param * Param * Param
  
type OpCode =
    | Halt = 99
    | Add = 1
    | Multiply = 2
    | Input = 3
    | Output = 4
    | Undefined = 0

type Instruction =
    { opCode: OpCode; opParams: Params }
    member this.Length
        with get() =
            match this.opParams with
            | Zilch -> 1
            | One _ -> 2
            | Three (_,_,_)  -> 4

//ABCDE
// 1002
//
//DE - two-digit opcode,      02 == opcode 2
// C - mode of 1st parameter,  0 == position mode
// B - mode of 2nd parameter,  1 == immediate mode
// A - mode of 3rd parameter,  0 == position mode,
//                                  omitted due to being a leading zero
let parseInst (inst: int) : OpCode * Mode[] =
    // TODO: Implement.
    (OpCode.Undefined, [|Mode.Position;Mode.Position;Mode.Position|])
    
    //let digits = toDigits inst
    //let opCode = digits.[] enum<OpCode>

let parse (program: Program) (pc: int) : Instruction =
    let (opCode, modes) = parseInst program.[pc]
    let def = { opCode = opCode; opParams = Zilch }
    match opCode with
    | OpCode.Halt -> def
    | OpCode.Input | OpCode.Output ->
        { def with opParams = One (modes.[0], program.[pc+1])  }
    | OpCode.Add | OpCode.Multiply ->
        { def with opParams = Three ((modes.[0], program.[pc+1]), (modes.[1], program.[pc+2]), (modes.[2], program.[pc+3])) }
    | _ -> failwith "Bad input format"

let eval (program: Program) (instruction: Instruction) =
    // TODO: Implement with new stuff
    match instruction.opCode with
    | Add op -> program.[op.OutAddress] <- program.[op.In1Address] + program.[op.In2Address]
    | Multiply op -> program.[op.OutAddress] <- program.[op.In1Address] * program.[op.In2Address]
    | _ -> ignore()
    
let run (program : Program) =
    let mutable pc = 0
    //let mutable instruction : Instruction = Undefined
    let mutable halt = false
    while not halt do
        let instruction = parse program pc 
        
        eval program instruction
        
        if instruction.OpCode = Halt then halt <- true
        
        pc <- pc + instruction.Length 
    program

// TESTS
// ...

// PROPER INPUT
let input : Program =
    (File.ReadAllText "day5-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse