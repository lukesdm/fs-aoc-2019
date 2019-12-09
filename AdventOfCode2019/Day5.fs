module AdventOfCode2019.Day5
open AdventOfCode2019
open Shared
open System.IO
open System.Reflection.Emit

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
            program.[dest] <- input
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
    
// TESTS

// 1002 -> 01002
let ``pad0 n less than input`` () =
    let expected = [|0; 1; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 2|]
    assert (expected = actual)

let ``pad0 n equals input`` () =
    let expected = [|1; 0; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 0; 2|]
    assert (expected = actual)
    
let ``pad0 n greater than input`` () =
    let expected = [|1; 0; 0; 0; 0; 2|]
    let actual = pad0 5 [|1; 0; 0; 0; 0; 2|]
    assert (expected = actual)

let ``parseInst example 1`` () =
    let input = 1002
    let expected = (OpCode.Multiply, [|ParamMode.Position; ParamMode.Immediate; ParamMode.Position|])
    let actual = parseInst input
    assert (expected = actual)

let ``multiply example 1`` () =
    let prog = parseProgDesc "1002,4,3,4,33"
    let expected = [|1002; 4; 3; 4; 99|]
    let actual = run prog 0 (new Output())
    assert (expected = actual)
    
// The program 3,0,4,0,99 outputs whatever it gets as input, then halts.
let ``io example 1`` () =
    let prog = parseProgDesc "3,0,4,0,99"
    let expected = [|42|]
    let output = new Output()
    let _ = run prog 42 output
    let actual = output.ToArray()
    assert (expected = actual)

// Tests from part 1
let runTests1 () =
    ``pad0 n less than input`` ()
    ``pad0 n equals input`` ()
    ``pad0 n greater than input`` ()
    ``parseInst example 1`` ()
    ``multiply example 1`` ()
    ``io example 1`` ()
    
let ``part 2 integration`` () =
    let progDesc = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"
    // The above example program uses an input instruction to ask for a single number.
    // The program will then output 999 if the input value is below 8,
    // output 1000 if the input value is equal to 8,
    // or output 1001 if the input value is greater than 8.
    let ``less than 8`` () =
        let prog = parseProgDesc progDesc
        let expected = [|999|]
        let output = new Output()
        let _ = run prog 7 output
        let actual = output.ToArray()
        assert (expected = actual)
    
    let ``equals 8`` () =
        let prog = parseProgDesc progDesc
        let expected = [|1000|]
        let output = new Output()
        let _ = run prog 8 output
        let actual = output.ToArray()
        assert (expected = actual)
    
    let ``greater than 8`` () =
        let prog = parseProgDesc progDesc
        let expected = [|1001|]
        let output = new Output()
        let _ = run prog 9 output
        let actual = output.ToArray()
        assert (expected = actual)
    
    ``less than 8``()
    ``equals 8``()
    ``greater than 8``()


// Tests for part 2
let runTests2 () =
    ``part 2 integration``()


// FOR REAL...

let patch (prog: Program) =
    // If required, initialize like so:
    // prog.[238] <- 135
    prog
    

    

// The TEST diagnostic program will start by requesting from the user the ID of the system to test by running an input instruction - provide it 1
let execute1 () =
    let prog = File.ReadAllText "Auxi\day5-input.txt" |> parseProgDesc |> patch
    let progInput = 1
    let output = new Output()
    let _ = run prog progInput output
    let diagnosticCode = output |> Seq.filter (fun o -> o <> 0) |> Seq.exactlyOne 
    printfn "Day 5 part 1 result = %d" diagnosticCode
    output.ToArray()
    
let execute2 () =
    let prog = File.ReadAllText "Auxi\day5-input.txt" |> parseProgDesc |> patch
    let progInput = 5
    let output = new Output()
    let _ = run prog progInput output
    let diagnosticCode = output |> Seq.filter (fun o -> o <> 0) |> Seq.exactlyOne 
    printfn "Day 5 part 2 result = %d" diagnosticCode
    output.ToArray()
    