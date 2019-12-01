module AdventOfCode2019.Day1Test
open AdventOfCode2019.Day1

let ``calcFuel1 happy path`` =
    let expected = 409
    let actual = calcFuel1 1234
    assert (expected = actual)
    
// from example provided - 1
let ``calcFuel2 small`` =
    let expected = 2
    let actual = calcFuel2 14
    assert (expected = actual)
    
// from example provided - 2
let ``calcFuel2 regular`` =
    let expected = 966
    let actual = calcFuel2 1969
    assert (expected = actual)
    
// from example provided - 3
let ``calcFuel2 regular2`` =
    let expected = 50346
    let actual = calcFuel2 100756
    assert (expected = actual)
    
   