﻿module AdventOfCode2019.Day2Test
open AdventOfCode2019.Day2

// Here are the initial and final states of a few more small programs:

//1,0,0,0,99 becomes 2,0,0,0,99 (1 + 1 = 2).
let ``Add test`` =
    let program : Program = [|1; 0; 0; 0; 99|]
    let expectedResult = [|2; 0; 0; 0; 99|]
    let actualResult = run program 
    assert (expectedResult = actualResult)
    
//2,3,0,3,99 becomes 2,3,0,6,99 (3 * 2 = 6).
let ``Multiply test 1`` =
    let program : Program = [|2; 3; 0; 3; 99|]
    let expectedResult = [|2; 3; 0; 6; 99|]
    let actualResult = run program 
    assert (expectedResult = actualResult)
    
//2,4,4,5,99,0 becomes 2,4,4,5,99,9801 (99 * 99 = 9801).
let ``Multiply test 2`` =
    let program : Program = [|2; 4; 4; 5; 99; 0|]
    let expectedResult = [|2; 4; 4; 5; 99; 9801|]
    let actualResult = run program 
    assert (expectedResult = actualResult)
    
//1,1,1,4,99,5,6,0,99 becomes 30,1,1,4,2,5,6,0,99.
let ``Combined test`` =
    let program : Program = [|1; 1; 1; 4; 99; 5; 6; 0; 99|]
    let expectedResult = [|30; 1; 1; 4; 2; 5; 6; 0; 99|]
    let actualResult = run program 
    assert (expectedResult = actualResult)