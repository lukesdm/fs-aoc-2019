module AdventOfCode2019.Day1
open System.IO

// Fuel required to launch a given module is based on its mass.
// Specifically, to find the fuel required for a module,
// take its mass, divide by three, round down, and subtract 2.

// What is the sum of the fuel requirements for all of the modules on your spacecraft?

let input =
    File.ReadAllLines "day1-input.txt"
    |> Array.map (fun s -> int s)

let calcFuel mass =
    (mass / 3) - 2 // int division will round down

let total = Array.sumBy calcFuel input