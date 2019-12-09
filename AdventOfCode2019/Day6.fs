module AdventOfCode2019.Day6
open System.Collections.Generic
// Universal Orbit Map
// https://adventofcode.com/2019/day/6

let tokenizeFile (desc: string) =
    desc.Replace("\r", "") // normalize line endings
        .Split("\n")

let tokenize (orbitDesc: string) =
    let ts = orbitDesc.Split(")")
    (ts.[0], ts.[1])
    
type ID = string
type Orbit = { id: ID; parentId: ID option; orbitCount: int }
type Orbits = Dictionary<ID, Orbit>

let parse (orbitDescs: string[]) =
    let orbits = new Orbits()  // TODO: use F# native map/dict type   
    let root = { id = "COM"; parentId = None; orbitCount = 0 }
    orbits.Add(root.id, root)
    orbitDescs
    |> Seq.map tokenize
    |> Seq.iter ( fun (parentId, id) ->
        let parent = orbits.[parentId]
        let orbit = { id = id; parentId = Some parentId; orbitCount = 1 + parent.orbitCount }
        orbits.Add(id, orbit)
        )
    orbits

let calcChecksum (orbits: Orbits) = 
    orbits.Values
    |> Seq.fold (fun total orbit -> total + orbit.orbitCount) 0

// TESTS
let ``tests - example`` () =
    let desc = "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L"
    
    let ``can tokenize - file`` () =
        let expected = 11
        let actual = tokenizeFile desc |> Seq.length
        assert (expected = actual)
        
    let ``can tokenize - line`` () =
        let line = "COM)B"
        let expected = ("COM", "B")
        let actual = tokenize line
        assert (expected = actual)
        
    let ``can calc checksum`` () =
        let expected = 42
        let actual = tokenizeFile desc |> parse |> calcChecksum
        assert (expected = actual)
        
    ``can tokenize - file`` ()
    ``can tokenize - line`` ()
    ``can calc checksum`` ()