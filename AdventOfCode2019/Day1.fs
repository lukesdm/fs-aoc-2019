module AdventOfCode2019.Day1
open System.IO
open System;

// PART 1:
// Fuel required to launch a given module is based on its mass.
// Specifically, to find the fuel required for a module,
// take its mass, divide by three, round down, and subtract 2.

// What is the sum of the fuel requirements for all of the modules on your spacecraft?

let input =
    File.ReadAllLines "Auxi\day1-input.txt"
    |> Array.map (fun s -> int s)

let calcFuel1 mass =
    (mass / 3) - 2 // int division will round down

let total1 = Array.sumBy calcFuel1 input

// PART 2:
// Apparently, you forgot to include additional fuel for the fuel you just added.
// Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2.
// However, that fuel also requires fuel, and that fuel requires fuel, and so on.
// Any mass that would require negative fuel should instead be treated as if it requires zero fuel
// A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel required is still just 2.
// At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
// The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.
// What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into account the mass of the added fuel? (Calculate the fuel requirements for each module separately, then add them all up at the end.)

// Original iterative version:
//let calcFuel2 moduleMass =
//    let calcFuelInc mass = (mass / 3) - 2
//    let mutable fuelInc = calcFuelInc moduleMass
//    let mutable fuelTotal = 0
//    while fuelInc > 0 do
//        fuelTotal <- fuelTotal + fuelInc
//        fuelInc <- calcFuelInc fuelInc
//    fuelTotal

// recursive attempt (not tail recursive)
//let calcFuel2 moduleMass =
//    let calcFuelInc mass = (mass / 3) - 2  
//    let rec calcFuelTotal mass =
//        let fuelMass = calcFuelInc mass
//        if fuelMass < 0 then 0 else
//            fuelMass + calcFuelTotal fuelMass
//    calcFuelTotal moduleMass
    
// tail recursive solution
let calcFuel2 moduleMass =
    let calcFuelInc mass = (mass / 3) - 2  
    let rec calcFuelTotal mass acc =
        let fuelMass = calcFuelInc mass
        if fuelMass < 0 then acc else
            calcFuelTotal fuelMass (acc + fuelMass)
    calcFuelTotal moduleMass 0

let total2 = Array.sumBy calcFuel2 input