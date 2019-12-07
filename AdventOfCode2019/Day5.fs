module AdventOfCode2019.Day5
open AdventOfCode2019
open Shared
open System.IO
open System.Reflection.Emit

let hello = "just need a reference to trigger the code in here."

// Program/memory
type Program = int[]
    
type ParamMode =
    | Position = 0
    | Immediate = 1
type Value = int
type Param = ParamMode * Value
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
let pad0 n (digits: int[]) =
    Array.append [|for _ in 1..(n-digits.Length) -> 0 |] digits

let parseInst (inst: int) : OpCode * ParamMode[] =
    let digits = toDigits inst |> pad0 5 // for now, always pad to 5 and ignore surplus modes 
    let opCode = enum<OpCode> (10*digits.[3] + digits.[4])
    let paramModes: ParamMode[] = digits.[..2] |> Array.map enum<ParamMode>
    (opCode, paramModes)

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

let getArg (p:Param) (mem: Program) =
    let (mode, v) = p
    match mode with
    | ParamMode.Immediate -> v
    | ParamMode.Position -> mem.[v]
    | _ -> failwith "Parameter mode unsupported."
 
let eval (program: Program) (instruction: Instruction) =
    // TODO: Implement with new stuff
    match (instruction.opCode, instruction.opParams) with
    | OpCode.Add, Three (o, in1, in2) ->
        let dest = getArg o program
        let arg1 = getArg in1 program
        let arg2 = getArg in2 program
        program.[dest] <- arg1 + arg2
    | Multiply op -> program.[op.OutAddress] <- program.[op.In1Address] * program.[op.In2Address]
    | _ -> ignore()
    
let run (program : Program) = // TODO: Input and Output
    let mutable pc = 0
    //let mutable instruction : Instruction = Undefined
    let mutable halt = false
    while not halt do
        let instruction = parse program pc 
        
        // TODO: Input and output
        eval program instruction
        
        if instruction.opCode = OpCode.Halt then halt <- true
        
        pc <- pc + instruction.Length 
    program

// TESTS

// 1002 -> 01002
let ``pad0 n less than input`` =
    let expected = [|0; 1; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 2|]
    assert (expected = actual)

let ``pad0 n equals input`` =
    let expected = [|1; 0; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 0; 2|]
    assert (expected = actual)
    
let ``pad0 n greater than input`` =
    let expected = [|1; 0; 0; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 0; 0; 2|]
    assert (expected = actual)

let ``parseInst example 1`` =
    let input = 1002
    let expected = (OpCode.Multiply, [|ParamMode.Position; ParamMode.Immediate; ParamMode.Position|])
    let actual = parseInst input
    assert (expected = actual)

// PROPER INPUT
let input : Program =
    (File.ReadAllText "day5-input.txt")
        .Split(",")
    |> Array.map System.Int32.Parse