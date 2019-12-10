module AdventOfCode2019.Day7
open System.Collections.Generic
open AdventOfCode2019
open Shared
open System
open System.IO

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
            program.[dest] <- input.Dequeue()  // (if input.Count > 1 then input.Dequeue() else input.Peek()) // TODO: check this is OK - should successive calls return same value by default?
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

// works for n <= 12 before hitting int32 limit
let factorial n = seq { for i in 1..n -> i } |> Seq.reduce (*)


// "...generates the next permutation lexicographically after a given permutation"
// Algorithm from https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order
let calcNextPerm (perm: int[]) = // TODO: Fix this - not working properly.
    let nextPerm = Array.copy perm
    let iMax = nextPerm.Length - 1
    //let mutable k: int = 0
    //let mutable l: int = 0
    
    //1. Find the largest index k such that a[k] < a[k + 1]. If no such index exists, the permutation is the last permutation.
//    while k + 1 <= maxIndex - 1 && nextPerm.[k] < nextPerm.[k + 1] do
//        k <- k + 1
    let k =
        seq { 0..(iMax - 1) }
        |> Seq.filter (fun k -> perm.[k] < perm.[k + 1])
        |> Seq.append [ System.Int32.MinValue ] // in case empty  
        |> Seq.max
      
    //2. Find the largest index l greater than k such that a[k] < a[l].
//    l <- k + 1
//    while l + 1 <= iMax && nextPerm.[k] < nextPerm.[l] do
//        l <- l + 1
    let l =
        seq { (k + 1)..iMax }
        |> Seq.filter (fun l -> perm.[k] < perm.[l])
        |> Seq.append [ System.Int32.MinValue ] // in case empty
        |> Seq.max
    
    // 3. Swap the value of a[k] with that of a[l].
    if l > 0 && k > 0 then
        let temp = nextPerm.[k]
        nextPerm.[k] <- nextPerm.[l]
        nextPerm.[l] <- temp
    
    //4. Reverse the sequence from a[k + 1] up to and including the final element a[n].
    Array.append (nextPerm.[..k]) (Array.rev nextPerm.[(k+1)..])

// TODO: get this working
let permutations (choices: int[]) : seq<int[]> =
    let permCount = factorial choices.Length
    let mutable currPerm = Array.sort choices
    
    seq {
        for _ in 1..permCount do
            yield currPerm
            currPerm <- calcNextPerm currPerm
    }

let maximise (program: Program) =
    
    let mutable maxResult = Int32.MinValue 
    
    Day7.Data.permutations
    |> Seq.iter (fun phases ->
        let result = runAll phases 0 program
        maxResult <- Math.Max(result, maxResult)
        )
    
    maxResult                

let runTests() =
    let progDesc1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    let ``example 1 - echo`` () =
        let phases = [| 4; 3; 2; 1; 0 |]
        let initialInput = 0
        let expectedOut = 43210
        let actualOut = parseProgDesc progDesc1 |> runAll phases initialInput
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
        
    let ``can maximise 1`` () =
        let expected = 43210
        let prog = parseProgDesc progDesc1
        let actual = maximise prog
        assert (expected = actual)
    
    let ``can find permutations`` () =
        let choices = [| 0; 1; 2 |]
        let expected = [|
            [|0; 1; 2|]
            [|0; 2; 1|]
            [|1; 0; 2|]
            [|1; 2; 0|]
            [|2; 0; 1|]
            [|2; 1; 0|]
        |]
        let actual = choices |> permutations |> Seq.toArray
        assert (expected = actual)
    
    ``example 1 - echo``()
    ``example 2``()
    ``example 3``()
    // ``can find permutations``() // TODO: Fix perm calc
    ``can maximise 1``()
    
    
let execute() =
    let prog = File.ReadAllText "Auxi\day7-input.txt" |> parseProgDesc
    let max = maximise prog
    printfn "Day 7 part 1 result: %d" max