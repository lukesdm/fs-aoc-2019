module AdventOfCode2019.Day4
open System

// The Elves had written the password on a sticky note, but someone threw it out.
//However, they do remember a few key facts about the password:
//It is a six-digit number.
//The value is within the range given in your puzzle input.
//Two adjacent digits are the same (like 22 in 122345).
//Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

let inRange n lower upper =
    lower <= n && n <= upper
    
//let hasDouble (n: int) =
//    let digits = string n
//    digits |> Seq.pairwise
//    |> Seq.filter ( fun a -> fst a = snd a )
//    |> Seq.isEmpty |> not
//
//let digitsDontDecrease (n: int) =
//    let digits = string n
//    digits |> Seq.pairwise
//    |> Seq.filter ( fun a -> snd a >= fst a )
//    |> Seq.isEmpty |> not
let toDigits (n: int) =
    // TODO: make this less ugly?
    string n |> Seq.map (fun c -> Int32.Parse (c.ToString ()))
    |> Seq.toArray

let checkDigits (n: int) =
    let digits = toDigits n
    let mutable hasDouble = false
    let mutable decreases = false
    digits |> Seq.pairwise
    |> Seq.iter (fun (prev, curr) ->
        if curr = prev then hasDouble <- true
        if curr < prev then decreases <- true)
    hasDouble && not decreases
    
let checkNumber n lower upper =
    inRange n lower upper
    && checkDigits n

// 111111 meets these criteria (double 11, never decreases)
let test1 =
    let n = 111111
    let expected = true
    let actual = checkNumber n 0 999999
    assert (actual = expected) 

//223450 does not meet these criteria (decreasing pair of digits 50).
let test2 =
    let n = 223450
    let expected = false
    let actual = checkNumber n 0 999999
    assert (actual = expected) 

// 123789 does not meet these criteria (no double)
let test3 =
    let n = 123789
    let expected = false
    let actual = checkNumber n 0 999999
    assert (actual = expected)
    
let tests =
    test1
    test2
    test3

// Your puzzle input is 172851-675869
let calc =
    let lower = 172851
    let upper = 675869
    let mutable validCount = 0
    seq {lower..upper}
    |> Seq.iter (fun n -> if checkDigits n then validCount <- validCount + 1)
    validCount
 
