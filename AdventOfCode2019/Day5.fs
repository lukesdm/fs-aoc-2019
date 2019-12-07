module AdventOfCode2019.Day5
open AdventOfCode2019
open Shared
open System.IO

let hello = "just need a reference to trigger the code in here."

// Program/memory
type Program = int[]

type Input = int
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
    let paramModes: ParamMode[] = digits.[..2] |> Array.rev |> Array.map enum<ParamMode>
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

let getArg (mem: Program) (p:Param) =
    let (mode, v) = p
    match mode with
    | ParamMode.Immediate -> v
    | ParamMode.Position -> mem.[v]
    | _ -> failwith "Parameter mode unsupported."
 
let eval (program: Program) (instruction: Instruction) (input: Input) (output: Output) =
    let getArg = getArg program // partial application
    match (instruction.opCode, instruction.opParams) with
    | opCode, Three (in1, in2, o) ->
        let dest = snd o //getArg o (see [Note])
        let arg1 = getArg in1
        let arg2 = getArg in2
        match opCode with
        | OpCode.Add -> program.[dest] <- arg1 + arg2
        | OpCode.Multiply -> program.[dest] <- arg1 * arg2
        | _ -> failwith "Unsupported opCode"
    | opCode, One (p) ->
        let loc = snd p //getArg p (see [Note])
        match opCode with
        | OpCode.Input -> program.[loc] <- input
        | OpCode.Output -> output.Add(program.[loc])
        | _ -> failwith "Unsupported opCode"
    | OpCode.Halt, Zilch -> ignore()
    | _ -> failwith "Unsupported opCode"

// [Note] "Parameters that an instruction writes to will never be in immediate mode."
    
let run (program : Program) (input: Input) (output: Output) =
    let mutable pc = 0
    let mutable halt = false
    while not halt do
        let instruction = parse program pc 
        
        // TODO: Input and output
        eval program instruction input output
        
        if instruction.opCode = OpCode.Halt then halt <- true
        
        pc <- pc + instruction.Length 
    program

let parseProgDesc (desc: string) : Program =
    desc.Split(",")
    |> Array.map System.Int32.Parse
    
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

let ``multiply example 1`` =
    let prog = parseProgDesc "1002,4,3,4,33"
    let expected = [|1002; 4; 3; 4; 99|]
    let actual = run prog 0 (new Output())
    assert (expected = actual)
    
// The program 3,0,4,0,99 outputs whatever it gets as input, then halts.
let ``io example 1`` =
    let prog = parseProgDesc "3,0,4,0,99"
    let expected = [|42|]
    let output = new Output()
    let _ = run prog 42 output
    let actual = output.ToArray()
    assert (expected = actual)

// FOR REAL...
// The TEST diagnostic program will start by requesting from the user the ID of the system to test by running an input instruction - provide it 1
let execute () =
    let prog = File.ReadAllText "day5-input.txt" |> parseProgDesc
    let progInput = 1
    let output = new Output()
    let _ = run prog progInput output
    output.ToArray()