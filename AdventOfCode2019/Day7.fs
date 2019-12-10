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

// BASED ON DAY 5
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
        
    (program, status, pc)

let parseProgDesc (desc: string) : Program =
    desc.Split(",")
    |> Array.map System.Int32.Parse
    
// *****

// At its first input instruction, provide it the amplifier's phase setting, 3.
// At its second input instruction, provide it the input signal, 0.

let runAll1 phases initial program =
    let mutable prevOut: int = initial
    phases |> Seq.iter (fun phase ->
        let inputBuff = new Input([ phase; prevOut ])
        let outputBuff = new Output()
        let _ = run (Array.copy program) inputBuff outputBuff 0 
        prevOut <- Seq.exactlyOne outputBuff
        )
    prevOut
    
type Phases = int[]

type Computer = { program: Program; input: Input; output: Output; mutable lastPc: int }  
let runAll2 (phases: Phases) program =
    let amps =
        phases
        |> Array.map (fun initIn ->
            { program = Array.copy program; input = new Input([| initIn |]); output = new Output(); lastPc = 0 } )
    
    let mutable lastAmpStatus = Running 
    let mutable prevOut = 0
    while lastAmpStatus <> Halted do
        amps |> Array.iter (fun computer ->
            computer.input.Enqueue(prevOut)
            let (_, status, pc) = run computer.program computer.input computer.output computer.lastPc
            computer.lastPc <- pc 
            lastAmpStatus <- status
            prevOut <- computer.output.Dequeue()
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

let maximise1 (program: Program) =
    let mutable maxResult = Int32.MinValue 
    Day7Data.permutations
    |> Seq.iter (fun phases ->
        let result = runAll1 phases 0 program
        maxResult <- Math.Max(result, maxResult)
        )
    maxResult

let maximise2 (program: Program) =
    Day7Data.permutations2
    |> Seq.fold (fun (max: int * Phases) phases ->
        let result = runAll2 phases program
        if result > fst max then (result, phases) else max
        ) (Int32.MinValue, Array.empty)

let runTests() =
    let progDesc1 = "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
    let ``part 1 example 1 - echo`` () =
        let phases = [| 4; 3; 2; 1; 0 |]
        let initialInput = 0
        let expectedOut = 43210
        let actualOut = parseProgDesc progDesc1 |> runAll1 phases initialInput
        assert (expectedOut = actualOut)
        
    let ``part 1 example 2`` () =
        let progDesc = "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0"
        let phases = [| 0; 1; 2; 3; 4 |]
        let initialInput = 0
        let expectedOut = 54321
        let actualOut = parseProgDesc progDesc |> runAll1 phases initialInput
        assert (expectedOut = actualOut)
        
    let ``part 1 example 3`` () =
        let progDesc = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
        let phases = [| 1; 0; 4; 3; 2 |]
        let initialInput = 0
        let expectedOut = 65210
        let actualOut = parseProgDesc progDesc |> runAll1 phases initialInput
        assert (expectedOut = actualOut)
        
    let ``part 1 - can maximise 1`` () =
        let expected = 43210
        let prog = parseProgDesc progDesc1
        let actual = maximise1 prog
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
        
    let ``part 2 example 1`` () =
        let progDesc = "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
        let expected = (139629729, [| 9; 8; 7; 6; 5 |]) // max signal, and phases that generate it 
        let actual = progDesc |> parseProgDesc |> maximise2
        assert (expected = actual)
        
    let ``part 2 example 2`` () =
        let progDesc = "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
        let expected = (18216, [| 9; 7; 8; 5; 6 |]) // max signal, and phases that generate it 
        let actual = progDesc |> parseProgDesc |> maximise2
        assert (expected = actual)
    
    ``part 1 example 1 - echo``()
    ``part 1 example 2``()
    ``part 1 example 3``()
    // ``can find permutations``() // TODO: Fix perm calc
    ``part 1 - can maximise 1``()
    
    ``part 2 example 1``()
    ``part 2 example 2``()
    
let execute1() =
    let prog = File.ReadAllText "Auxi\day7-input.txt" |> parseProgDesc
    let max = maximise1 prog
    printfn "Day 7 part 1 result: %d" max
    
let execute2() =
    let prog = File.ReadAllText "Auxi\day7-input.txt" |> parseProgDesc
    let (max,_) = maximise2 prog
    printfn "Day 7 part 2 result: %d" max // 44282086 confirmed as correct