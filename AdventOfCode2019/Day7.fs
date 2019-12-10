module AdventOfCode2019.Day7
open System.Collections.Generic
open System.Collections.Generic
open AdventOfCode2019
open Shared

// Day 7: Amplification Circuit
// https://adventofcode.com/2019/day/7

// 5 amplifiers connected in series, running IntCode program.
// Maximise final output signal given each a 'phase' input in range 0-4

// For each amplifier
//  set input
//    [phase param; prev.output]
//  run prog
//  get output

// FROM DAY 5
// *****

// Program/memory
type Program = int[]

// type Input = int
type Input = Queue<int>
type Output = ResizeArray<int> 
//type Computer = Program * Input * Output
    
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
            | Two (_,_) -> 3 // TODO: can simplify to single _ ?
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
    let mutable pcOverride: int option = None
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
            if test <> 0 then pcOverride <- Some newPc
        | OpCode.JumpIfFalse ->
            if test = 0 then pcOverride <- Some newPc
        | _ -> failwith "Unsupported opCode"
    | opCode, One (param) ->
        match opCode with
        | OpCode.Input ->
            let dest = snd param //NOT getArg p (see [Note])
            program.[dest] <- input.Dequeue() // TODO: check this is OK - should successive calls return same value by default?
        | OpCode.Output ->
            let value = getArg param
            output.Add(value)
        | _ -> failwith "Unsupported opCode"
    | OpCode.Halt, Zilch -> ignore()
    | _ -> failwith "Unsupported opCode"
    
    pcOverride

// [Note] "Parameters that an instruction writes to will never be in immediate mode."
    
let run (program : Program) (input: Input) (output: Output) =
    let mutable pc = 0
    let mutable halt = false
    while not halt do
        let instruction = parse program pc 
        
        let pcOverride = eval program instruction input output
        
        if instruction.opCode = OpCode.Halt then halt <- true
        
        pc <- if pcOverride.IsNone then (pc + instruction.Length) else pcOverride.Value 
    program

let parseProgDesc (desc: string) : Program =
    desc.Split(",")
    |> Array.map System.Int32.Parse
    
// *****

// At its first input instruction, provide it the amplifier's phase setting, 3.
// At its second input instruction, provide it the input signal, 0.

//type Amplifier = { program: Program; inSignal: int; inPhase: int; output: Output } 
let runAll phases initial program =
    let mutable prevOut: int = initial
    phases |> Seq.iter (fun phase ->
        //let ampl = { program = program; inSignal = inSig; inPhase = phase; output = new Output() }
        let inputBuff = new Input([ phase; prevOut ])
        let outputBuff = new Output()
        let _ = run (Array.copy program) inputBuff outputBuff
        prevOut <- Seq.exactlyOne outputBuff
        )
    prevOut

let runTests() =
    let ``example 1 - echo`` () =
        let progDesc = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
        let phases = [| 4; 3; 2; 1; 0 |]
        let initialInput = 0
        let expectedOut = 43210
        let actualOut = parseProgDesc progDesc |> runAll phases initialInput
        assert (expectedOut = actualOut)
        
    let ``example 2`` () =
        let progDesc = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        let phases = [| 0; 1; 2; 3; 4 |]
        let initialInput = 0
        let expectedOut = 54321
        let actualOut = parseProgDesc progDesc |> runAll phases initialInput
        assert (expectedOut = actualOut)
        
    let ``example 3`` () =
        let progDesc = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        let phases = [| 1; 0; 4; 3; 2 |]
        let initialInput = 0
        let expectedOut = 65210
        let actualOut = parseProgDesc progDesc |> runAll phases initialInput
        assert (expectedOut = actualOut)
    
    ``example 1 - echo``()
    ``example 2``()
    ``example 3``()
    
        

