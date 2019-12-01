module AdventOfCode2019.Day1Test
open AdventOfCode2019.Day1

let ``calcFuel1 happy path`` =
    let expected = 409
    let actual = calcFuel1 1234
    assert (expected = actual)
    
let ``calcFuel1 fail`` = // example test fail
    let expected = 407
    let actual = calcFuel1 1234
    assert (expected = actual)