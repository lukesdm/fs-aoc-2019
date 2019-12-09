module AdventOfCode2019.Day6
open System.Collections.Generic
open System;
open System.IO

// Universal Orbit Map
// https://adventofcode.com/2019/day/6

let tokenizeFile (desc: string) =
    desc.Replace("\r", "") // normalize line endings
        .Split("\n")

let tokenize (orbitDesc: string) =
    let ts = orbitDesc.Split(")")
    (ts.[0], ts.[1])
    
type ID = string
type Orbit = { id: ID; parentId: ID option; mutable orbitCount: int option }
type Orbits = Dictionary<ID, Orbit>

// TODO: Improve this by updating orbit counts in place
let calcOrbitCount (orbits: Orbits) (orbit: Orbit) =
    let rec countOrbits (orbit: Orbit) count =
        if orbit.parentId.IsNone then count // COM root
        else
            countOrbits orbits.[orbit.parentId.Value] (count + 1)
    countOrbits orbit 0 

let updateOrbitCounts (orbits: Orbits) =
    let calcOrbitCount = calcOrbitCount orbits
    orbits.Values |> Seq.iter (fun orbit ->
        orbit.orbitCount <- Some (calcOrbitCount orbit)
        )

let parse (orbitDescs: string[]) =
    let orbits = new Orbits()
    let root = { id = "COM"; parentId = None; orbitCount = Some 0 }
    orbits.Add(root.id, root)
    orbitDescs
    |> Seq.map tokenize
    |> Seq.iter ( fun (parentId, id) ->
        let orbit = { id = id; parentId = Some parentId; orbitCount = None }
        orbits.Add(id, orbit)
        )
    
    updateOrbitCounts orbits
    
    orbits
    
// recursive attempt - terrible performance (suspect Map.Add)
//type Orbits = Map<ID, Orbit>
//let parse (orbitDescs: seq<string>) =
//let parse (orbitDescs: string[]) =
//    let root = { id = "COM"; parentId = None; } 
//    let rec parseI orbitDescs (orbits: Orbits) =
//        if Seq.isEmpty orbitDescs then
//            orbits
//        else
//            let parentId, id = Seq.head orbitDescs |> tokenize
//            let orbit = { id = id; parentId = Some parentId; } 
//            printfn "T = %A it = %d" System.DateTime.Now it
//            parseI (Seq.tail orbitDescs) (orbits.Add(orbit.id, orbit))
//    parseI orbitDescs (Map.ofList [root.id, root])



// memoization attempt - returns wrong results TODO: Fix
//type OrbitCounts = Map<ID, int>
//let calcOrbitCounts (orbits: Orbits) : OrbitCounts =
//    let rec countOrbits (orbit: Orbit) (orbitCounts: OrbitCounts) =
//        if orbit.parentId.IsNone then orbitCounts // root/COM
//        else
//            let parent = orbits.[orbit.parentId.Value]
//            let parentCount = Map.tryFind parent.id orbitCounts
//            if parentCount.IsSome then orbitCounts.Add (orbit.id, 1 + parentCount.Value)
//            else countOrbits parent orbitCounts
//    
//    let orbitCounts = orbits.Values |> Seq.fold ( fun (orbitCountsAcc: OrbitCounts) orbit ->
//            countOrbits orbit orbitCountsAcc ) (Map.ofList [ ("COM", 0) ])
//    orbitCounts
//    
//let calcChecksum (orbits: Orbits) = 
//    orbits |> calcOrbitCounts
//    |> Map.fold ( fun total _ count -> total + count ) 0

let calcChecksum (orbits: Orbits) = 
//    orbits
//    |> Seq.fold (fun total kvp -> total + (calcOrbitCount orbits kvp.Value)) 0
    orbits.Values
    |> Seq.map (fun orbit -> orbit.orbitCount.Value)
    |> Seq.sum

// PART 2
// What is the minimum number of orbital transfers required to move from the object YOU are orbiting
//  to the object SAN is orbiting? (Between the objects they are orbiting - not between YOU and SAN.)
// [ --> Distance from Nearest Common Ancestor]

let getAncestors (orbits: Orbits) (orbit: Orbit) =
    let rec getAncestors (orbit: Orbit) (ancestors: Set<Orbit>) =
        if orbit.parentId.IsNone then
            ancestors
        else
            let parent = orbits.[orbit.parentId.Value]
            getAncestors parent (ancestors.Add(parent))
    
    getAncestors orbit Set.empty
    
let calcHops orbits = 
    
    let calcHops (orbits: Orbits) (a: Orbit) (b: Orbit) =
        let getAncestors = getAncestors orbits // partial application
        let commonAncestors = Set.intersect (getAncestors a) (getAncestors b)
        let distNearestCommonAncestor =
            commonAncestors
            |> Set.fold (fun (max: int) (ancestor: Orbit) ->
                Math.Max(ancestor.orbitCount.Value, max) ) 0  
        let hopCount = Math.Abs(a.orbitCount.Value - distNearestCommonAncestor - 1)
                       + Math.Abs(b.orbitCount.Value - distNearestCommonAncestor - 1)
        hopCount
    calcHops orbits orbits.["SAN"] orbits.["YOU"]

// TESTS
let ``tests - example part 1`` () =
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
    
let ``tests - example part 2`` () =
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
K)L
K)YOU
I)SAN"
    let ``can calc hops`` () =
        let expected = 4
        let actual = desc |> tokenizeFile |> parse |> calcHops
        assert (expected = actual)
        
    ``can calc hops``()


let execute1 () =
    let result =
        File.ReadAllText ("Auxi\day6-input.txt")
        |> tokenizeFile
        |> parse
        |> calcChecksum
        
    printfn "Day 6 part 1 result: %d" result // 301100 confirmed correct.
    
let execute2 () =
    let result =
        File.ReadAllText ("Auxi\day6-input.txt")
        |> tokenizeFile
        |> parse
        |> calcHops
        
    printfn "Day 6 part 2 result: %d" result // 547 confirmed correct