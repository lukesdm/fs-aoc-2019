module AdventOfCode2019.Day8
open AdventOfCode2019
open System.IO
// Space Image Format
// https://adventofcode.com/2019/day/8

// Note - inefficient implementation here - could calc checksums without storing everything in memory
 
type Layer = int[,]
type Image = seq<Layer>
let parse (width: int) (height: int) (imageDesc: string) : Image =
    let createLayer (inp: int[]) = // int[] -> int[row,col]
        Array2D.init height width ( fun row col -> inp.[col + (row * width)] ) 
         
    let imageRaw = Shared.stringToDigits imageDesc
    let layers =
        imageRaw
        |> Seq.chunkBySize (width * height)
        |> Seq.map createLayer
    
    layers
    
// Find the layer that contains the fewest 0 digits.
// On that layer, what is the number of 1 digits multiplied by the number of 2 digits?
let calcChecksum (image: Image) = 
    let countN (n: int) (layer: Layer)  =
        // not a much better way (poss rec but still not great)
        let mutable count = 0
        layer
        |> Array2D.iter (fun v ->
            if v = n then count <- count + 1
            )
        count
        
    let checkSum layer =
        (countN 1 layer) * (countN 2 layer)
    
    image
        |> Seq.minBy (countN 0)
        |> checkSum
// TESTS
//For example, given an image 3 pixels wide and 2 pixels tall,
// the image data 123456789012 corresponds to the following image layers:
//Layer 1: 123
//         456
//
//Layer 2: 789
//         012

// F# notes...
// A multidimensional array can be created, but there is no syntax for writing a multidimensional array literal.
// Use the operator array2D to create an array from a sequence of sequences of array elements.
// The sequences can be array or list literals.

let ``can make 2d array`` () =
    let array = array2D [| [|1; 2; 3|]; [|4; 5; 6|] |]
    let expected = 6
    let row, col = (1, 2)
    let actual = array.[row,col]
    assert (expected = actual)

let ``can parse 1`` () =
    let input = "123456789012"
    let expected =
        [|
            array2D [| [|1; 2; 3|]; [|4; 5; 6|] |]
            array2D [| [|7; 8; 9|]; [|0; 1; 2|] |]
        |]
    let actual = input |> parse 3 2 |> Seq.toArray
    assert (expected = actual)

let ``can calc checksum`` () =
    let layers = parse 3 2 "123456789012"
    let expected = 1
    let actual = calcChecksum layers
    assert (expected = actual)

let runTests () =
    ``can parse 1``()
    ``can calc checksum``()
    
let execute () =
    let image = File.ReadAllText "day8-input.txt" |> parse 25 6
    let cs = calcChecksum image
    printfn "Day 8 part 1 result: %d" cs
    