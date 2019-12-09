module AdventOfCode2019.Day4
open System
open Shared

let hello = "just need a reference to trigger the code in here."
// The Elves had written the password on a sticky note, but someone threw it out.
//However, they do remember a few key facts about the password:
//It is a six-digit number.
//The value is within the range given in your puzzle input.
//Two adjacent digits are the same (like 22 in 122345).
//Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).

let inRange n lower upper =
    lower <= n && n <= upper

let checkDigits1 (n: int) =
    let digits = intToDigits n
    let mutable hasDouble = false
    let mutable decreases = false
    digits |> Seq.pairwise
    |> Seq.iter (fun (prev, curr) ->
        if curr = prev then hasDouble <- true
        if curr < prev then decreases <- true)
    hasDouble && not decreases
    
// 111111 meets these criteria (double 11, never decreases)
let test1 =
    let n = 111111
    let expected = true
    let actual = checkDigits1 n
    assert (actual = expected) 

//223450 does not meet these criteria (decreasing pair of digits 50).
let test2 =
    let n = 223450
    let expected = false
    let actual = checkDigits1 n
    assert (actual = expected) 

// 123789 does not meet these criteria (no double)
let test3 =
    let n = 123789
    let expected = false
    let actual = checkDigits1 n
    assert (actual = expected)

// Your puzzle input is 172851-675869
let calc1 =
    let lower = 172851
    let upper = 675869
    let mutable validCount = 0
    seq {lower..upper}
    |> Seq.iter (fun n -> if checkDigits1 n then validCount <- validCount + 1)
    validCount
 
// PART 2:
//the two adjacent matching digits are not part of a larger group of matching digits
let checkDigits2 (n: int) =
    let digits = intToDigits n
    
    let mutable decreases = false
    
    // Note: this data structure + mechanism wouldn't work for 221222 (the triple would hide the double)
    // BUT it's not an issue because it's not valid anyway - the number can't decrease!
    let digitReps: int[] = Array.zeroCreate 10 // reps for digits 0 to 9
    
    digits |> Seq.pairwise
    |> Seq.iter (fun (prev, curr) ->
        if curr = prev then
            digitReps.[curr] <- digitReps.[curr] + 1 
        if curr < prev then decreases <- true)
    (Seq.exists (fun r -> r = 1) digitReps) && not decreases

let test1a =
    let n = 111111
    let expected = false
    let actual = checkDigits2 n
    assert (actual = expected) 

let test2a =
    let n = 223450
    let expected = false
    let actual = checkDigits2 n
    assert (actual = expected) 

let test3a =
    let n = 123789
    let expected = false
    let actual = checkDigits2 n
    assert (actual = expected)
    
//112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
let test4 =
    let n = 112233
    let expected = true
    let actual = checkDigits2 n
    assert (actual = expected)
//123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
let test5 =
    let n = 123444
    let expected = false
    let actual = checkDigits2 n
    assert (actual = expected)
//111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
let test6 =
    let n = 111122
    let expected = true
    let actual = checkDigits2 n
    assert (actual = expected)

let calc2 =
    let lower = 172851
    let upper = 675869
    let mutable validCount = 0
    seq {lower..upper}
    |> Seq.iter (fun n -> if checkDigits2 n then validCount <- validCount + 1)
    validCount