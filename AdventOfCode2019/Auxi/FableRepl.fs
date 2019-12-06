// https://fable.io/repl/
module BasicCanvas

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser

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
    
    //drawLine ctx a (25.,0.) (25.,50.)
    //drawLine ctx b (0.,25.) (50.,25.)
    
    //ctx.scale (0.3, 0.3)
    drawLine ctx a (0,0) (50,50)
    drawLine ctx b (0,50) (50,0)

init()


