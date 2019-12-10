module AdventOfCode2019.Day9
open System.Collections.Generic
open Shared
open System
open System.IO

// Day 9: Sensor Boost
// https://adventofcode.com/2019/day/9


// BASED ON DAY 7
// *****

// Program/memory
type Program = int[]

type Input = Queue<int>
type Output = Queue<int>
    
type ParamMode =
    | Position = 0
    | Immediate = 1
type Value = int
type Param = ParamMode * Value
type Params =
    | Zilch // Zero (avoid naming conflict)
    | One of Param
    | Two of Param * Param
    | Three of Param * Param * Param
  
type OpCode =
    | Halt = 99
    | Add = 1
    | Multiply = 2
    | Input = 3
    | Output = 4
    | JumpIfTrue = 5
    | JumpIfFalse = 6
    | LessThan = 7
    | Equals = 8
    | Undefined = 0

type Instruction =
    { opCode: OpCode; opParams: Params }
    member this.Length
        with get() =
            match this.opParams with
            | Zilch -> 1
            | One _ -> 2
            | Two _ -> 3
            | Three _  -> 4

type Status =
    | Running
    | WaitingForInput
    | Halted
    
type NewState =
    | PCOverride of int
    | Status of Status

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
    let digits = intToDigits inst |> pad0 5 // for now, always pad to 5 and ignore surplus modes 
    let opCode = enum<OpCode> (10*digits.[3] + digits.[4])
    let paramModes: ParamMode[] = digits.[..2] |> Array.rev |> Array.map enum<ParamMode>
    (opCode, paramModes)

let parse (program: Program) (pc: int) : Instruction =
    let (opCode, modes) = parseInst program.[pc]
    let def = { opCode = opCode; opParams = Zilch }
    match opCode with
    | OpCode.Halt -> def
    | OpCode.Input | OpCode.Output ->
        { def with opParams = One (modes.[0], program.[pc+1])  }
    | OpCode.JumpIfFalse | OpCode.JumpIfTrue ->
        { def with opParams = Two ((modes.[0], program.[pc+1]), (modes.[1], program.[pc+2])) }
    | OpCode.Add | OpCode.Multiply | OpCode.LessThan | OpCode.Equals ->
        { def with opParams = Three ((modes.[0], program.[pc+1]), (modes.[1], program.[pc+2]), (modes.[2], program.[pc+3])) }
    | _ -> failwith "Bad input format"

let getArg (mem: Program) (p:Param) =
    let (mode, v) = p
    match mode with
    | ParamMode.Immediate -> v
    | ParamMode.Position -> mem.[v]
    | _ -> failwith "Parameter mode unsupported."
 
let eval (program: Program) (instruction: Instruction) (input: Input) (output: Output) =
    let mutable newState = None
    let getArg = getArg program // partial application
    match (instruction.opCode, instruction.opParams) with
    | opCode, Three (in1Param, in2Param, outParam) ->
        let dest = snd outParam //NOT getArg o (see [Note])
        let in1Val = getArg in1Param
        let in2Val = getArg in2Param
        match opCode with
        | OpCode.Add -> program.[dest] <- in1Val + in2Val
        | OpCode.Multiply -> program.[dest] <- in1Val * in2Val
        | OpCode.LessThan -> program.[dest] <- if in1Val < in2Val then 1 else 0
        | OpCode.Equals -> program.[dest] <- if in1Val = in2Val then 1 else 0 
        | _ -> failwith "Unsupported opCode"
    | opCode, Two (param1, param2) ->
        let test = getArg param1
        let newPc = getArg param2
        match opCode with
        | OpCode.JumpIfTrue ->
            if test <> 0 then newState <- Some (PCOverride newPc)
        | OpCode.JumpIfFalse ->
            if test = 0 then newState <- Some (PCOverride newPc)
        | _ -> failwith "Unsupported opCode"
    | opCode, One (param) ->
        match opCode with
        | OpCode.Input ->
            let dest = snd param //NOT getArg p (see [Note])
            if input.Count > 0 then
                program.[dest] <- input.Dequeue()
            else
                newState <- Some (Status WaitingForInput)
        | OpCode.Output ->
            let value = getArg param
            output.Enqueue(value)
        | _ -> failwith "Unsupported opCode"
    | OpCode.Halt, Zilch -> newState <- Some (Status Halted)
    | _ -> failwith "Unsupported opCode"
    
    newState

// [Note] "Parameters that an instruction writes to will never be in immediate mode."

let run (program : Program) (input: Input) (output: Output) initPc =
    let mutable pc = initPc
    let mutable status = Running

    while status = Running do
        let instruction = parse program pc
        
        let newState = eval program instruction input output
        
        match newState with
        | Some (PCOverride pcO) -> pc <- pcO 
        | Some (Status s) -> status <- s
        | None -> pc <- (pc + instruction.Length)
        
    (status, pc)

let parseProgDesc (desc: string) : Program =
    desc.Split(",")
    |> Array.map System.Int32.Parse
    
// *****

let runTests() =
    let ``example 1 - quine`` () =
        // "takes no input and produces a copy of itself as output"
        let prog =
            "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
            |> parseProgDesc
        let expected = [| 109; 1; 204; -1; 1001; 100; 1; 100; 1008; 100; 16; 101; 1006; 101; 0; 99 |]
        let output = new Output()
        let _ = run prog (new Input()) output 0
        let actual = output.ToArray()
        assert (expected = actual)
    
    ``example 1 - quine``()
    
let execute() =
    let prog = File.ReadAllText "Auxi\day9-input.txt" |> parseProgDesc
    printfn "Day 9 part 1 result: %d" 0