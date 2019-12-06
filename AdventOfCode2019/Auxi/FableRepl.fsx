module BasicCanvas

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser
open System
open System.Text.RegularExpressions

type Point = {x: int; y: int}

let isBetween x n1 n2 =
    (n1 <= x && x <= n2)
    || (n2 <= x && x <= n1)

type Orientation =
    | Horizontal
    | Vertical

type WireSegment2 =
    { p1: Point;
      p2: Point;
      split: bool // for debugging 
      plStart: int
      plEnd: int } // path length at end of segment
    member this.orientation
        with get() =
            if this.p1.x = this.p2.x then Vertical else Horizontal
    member this.length
        with get() =
            match this.orientation with
            | Horizontal -> Math.Abs (this.p2.x - this.p1.x)
            | Vertical -> Math.Abs (this.p2.y - this.p1.y)
                           
type Wire2 = ResizeArray<WireSegment2> // supports additional entries due to segment splitting
type Intersection = { p: Point; segmentA: WireSegment2; segmentB: WireSegment2 }

let calcIntersection2 (segA: WireSegment2) (segB: WireSegment2) = 
    let intersects =
            (isBetween segB.p1.x segA.p1.x segA.p2.x
            && isBetween segB.p2.x segA.p1.x segA.p2.x
            && isBetween segA.p1.y segB.p1.y segB.p2.y
            && isBetween segA.p2.y segB.p1.y segB.p2.y)
            ||
            (isBetween segB.p1.y segA.p1.y segA.p2.y
            && isBetween segB.p2.y segA.p1.y segA.p2.y
            && isBetween segA.p1.x segB.p1.x segB.p2.x
            && isBetween segA.p2.x segB.p1.x segB.p2.x)

    if intersects then
        if segA.p1.y = segA.p2.y
        then Some { p = {x = segB.p1.x; y = segA.p1.y}; segmentA = segA; segmentB = segB }
        else Some { p = {x = segA.p1.x; y = segB.p1.y}; segmentA = segA; segmentB = segB }
    else None

let rec findIntersections (wireSegs: list<WireSegment2>) (newSeg: WireSegment2) (intersections) =
    if not wireSegs.IsEmpty then
        // TODO: potential issue with using newSeg here - will have inaccurate pathLength on subsequent calls - to fix would need to pass in result of split. Shouldn't be an issue if that seg is not used in calcs
        let intersection = calcIntersection2 newSeg wireSegs.Head
        if intersection.IsSome then
            findIntersections wireSegs.Tail newSeg (intersection.Value :: intersections)
        else
            findIntersections wireSegs.Tail newSeg intersections
    else
        intersections

let checkSelfIntersection (wire: Wire2) (newSegment: WireSegment2) =
    // Note: need to consider multiple intersections, so traverse all segments
    
    // hacky way to remove last segment so it's not counted as an intersection 
    let input =
        if wire.Count > 0 then
            ((Seq.toList wire |> List.rev).Tail |> List.rev)
        else
            list.Empty
    
    findIntersections input newSegment List.empty

let dist p1 p2 =
    if p1.x = p2.x then Math.Abs (p2.y - p1.y)
    elif p1.y = p2.y then Math.Abs (p1.x - p2.x)
    else failwith "expected a horizontal or vertical distance"
    
let calcNewPathLength (segA: WireSegment2) (intersection: Intersection)  =
    // when segA is intercepted by segB, take segBs pathlength (which will be shorter)
    //  minus the distance along the segment from the intersection  
    let segB = intersection.segmentB
    let distFromEnd = dist segB.p2 intersection.p
    segB.plEnd - distFromEnd  
    
let split origSegment intersections =
    // Split segment according to intersections, assigning new pathlength
    let mutable currSeg = origSegment
    let mutable segs: list<WireSegment2> = list.Empty
    
    intersections |> Seq.iter ( fun intersection ->
        let newPathLength = calcNewPathLength currSeg intersection 
        let newSeg = { p1 = currSeg.p1; p2 = intersection.p; plStart = currSeg.plStart; plEnd = newPathLength; split = true  }
        currSeg <- newSeg
        segs <- newSeg :: segs )
    let finalSeg =
        { p1 = currSeg.p2;
          p2 = origSegment.p2;
          split = true
          plStart = currSeg.plEnd
          plEnd = currSeg.plEnd + (dist currSeg.p2 origSegment.p2) }
    segs <- finalSeg :: segs 
    List.rev segs

type SegsAddedCallback = list<WireSegment2>->unit
let parseWireDescription2 (input: string) (segsAddedCallback: option<SegsAddedCallback>) =
    let regex = new Regex "(U|D|L|R)(\d+)" // grp1 = dir, grp2 = amount
    let mutable prevSeg : WireSegment2 = {p1 = {x=0; y=0}; p2={x=0; y=0;}; plStart = 0; plEnd = 0;  split = false}
    let tokens = input.Split(",")
    let segments = new Wire2 (tokens.Length) // we know it's at least as big as this
    tokens |> Array.iter (fun token ->
        let rmatch = regex.Match token 
        let (dir, length) = rmatch.Groups.[1].Value, int rmatch.Groups.[2].Value
        let plEnd = prevSeg.plEnd + length
        
        let newP2 =
            match dir with
            | "U" -> { prevSeg.p2 with y = (prevSeg.p2.y + length) }
            | "D" -> { prevSeg.p2 with y = (prevSeg.p2.y - length) }
            | "R" -> { prevSeg.p2 with x = (prevSeg.p2.x + length) }
            | "L" -> { prevSeg.p2 with x = (prevSeg.p2.x - length) }
            | _ -> failwith "Unexpected input"
        
        let newSegment = { p1 = prevSeg.p2; p2 = newP2; plStart = prevSeg.plEnd; plEnd = plEnd; split = false }
        
        let selfIntersections = checkSelfIntersection segments newSegment 
        
        let newSegs =
            if selfIntersections.IsEmpty then
                [newSegment]
            else
                split newSegment selfIntersections
        
        segments.AddRange newSegs
        if segsAddedCallback.IsSome then
           segsAddedCallback.Value newSegs
        prevSeg <- (List.rev newSegs).Head
        )
    segments

// x,y coords of lower left and top right corners 
type Rect = ((int*int) * (int*int))

let getBounds wire : Rect =
    // TODO: figure out how to extract calculated min to avoid reevaluating afterwards
    let fminX seg = Math.Min (seg.p1.x, seg.p2.x)
    let fminY seg = Math.Min (seg.p1.y, seg.p2.y)
    let fmaxX seg = Math.Max (seg.p1.x, seg.p2.x)
    let fmaxY seg = Math.Max (seg.p1.y, seg.p2.y)
    let minX = Seq.minBy fminX wire
    let minY = Seq.minBy fminY wire
    let maxX = Seq.maxBy fmaxX wire
    let maxY = Seq.maxBy fmaxY wire
    (fminX minX, fminY minY), (fmaxX maxX, fmaxY maxY)

// for viz - gets scale and offsets required to fit wire onto canvas
let getTransform srcRect destRect =
    // return a scale and a shift
    // o = origin, c = top-right corner
    let (ox1, oy1), (cx1, cy1) = srcRect
    let (ox2, oy2), (cx2, cy2) = destRect
    let w1, h1 = cx1-ox1, cy1-oy1
    let w2, h2 = cx2-ox2, cy2-oy2
    let scaleX, scaleY = float w2 / w1, float h2 / h1
    let offsetX, offsetY = float ox2-ox1, float oy2-oy1
    (scaleX, scaleY), (offsetX, offsetY)

let drawLine (ctx: CanvasRenderingContext2D) style (x1,y1) (x2,y2) =
    ctx.strokeStyle <- style
    ctx.beginPath ()
    ctx.moveTo (float x1, float y1)
    ctx.lineTo (float x2, float y2)
    ctx.stroke()

let init() =
    let canvas = document.querySelector(".view") :?> HTMLCanvasElement

    let ctx = canvas.getContext_2d()
    // The (!^) operator checks and casts a value to an Erased Union type
    // See http://fable.io/docs/interacting.html#Erase-attribute
    
    // Line styles
    let a = !^"rgba(200, 0, 0, 0.7)"
    let b = !^"rgba(0, 0, 200, 0.7)"
    
    //ctx.scale (0.5, 0.5)

    // hardcode for now
    // For real input = ((-4662, -4556), (9284, 7720))
    // For test input starting R75 = ((0, -30), (217, 53))
    let wireRect = ((0., -30.), (217., 53.))
    
    //let canvasRect = ((0.,0.),(canvas.width, canvas.height))
    // TODO: Fix this - scaling and flip
    let canvasRect = ((10.,10.),(canvas.width-10., canvas.height-10.))
    let (scaleX, scaleY), (shiftX, shiftY) = getTransform wireRect canvasRect
    
    // let shiftX = 100.
    // let shiftY = 100.
    // let scaleX = 0.5
    // let scaleY = 0.5

    let drawSeg seg style = 
        drawLine ctx style ((float seg.p1.x + shiftX) * scaleX, (float seg.p1.y + shiftY) * scaleY) ((float seg.p2.x + shiftX) * scaleX, (float seg.p2.y + shiftY) * scaleY)

    let segsAddedCallbackA: SegsAddedCallback =
        fun segs -> 
            Seq.iter (fun seg -> drawSeg seg a) segs

    let segsAddedCallbackB: SegsAddedCallback =
        fun segs -> 
            Seq.iter (fun seg -> drawSeg seg b) segs

    let wireA = parseWireDescription2 "R75,D30,R83,U83,L12,D49,R71,U7,L72" (Some segsAddedCallbackA)
    let wireB = parseWireDescription2 "U62,R66,U55,R34,D71,R55,D58,R83" (Some segsAddedCallbackB)
    
    printf "canvas w=%A h=%A" ctx.canvas.width ctx.canvas.height

    ()
    //drawLine ctx a (0,0) (50,50)
    //drawLine ctx b (0,50) (50,0)

init()
