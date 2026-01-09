module public PlotBuffer

open System.Collections.Generic

type PlotPoint = {
    X: float
    Y: float
}

type InterpolationType = 
    | Linear
    | Spline

// Mutable buffer to store plot points
let mutable private points: List<PlotPoint> = new List<PlotPoint>()
let mutable private interpolation: InterpolationType = Linear

// Add a point to the plot
let addPoint (x: float) (y: float) : unit =
    points.Add({ X = x; Y = y })

// Set interpolation type
let setInterpolation (interpType: InterpolationType) : unit =
    interpolation <- interpType

// Get all points
let getPoints () : PlotPoint list =
    points |> Seq.toList

// Get interpolation type
let getInterpolation () : InterpolationType =
    interpolation

// Clear all points
let clear () : unit =
    points.Clear()
    interpolation <- Linear

// Interpolation functions
let linearInterp (x1: float) (y1: float) (x2: float) (y2: float) (x: float) : float =
    if abs(x2 - x1) < 1e-10 then y1
    else y1 + (y2 - y1) * (x - x1) / (x2 - x1)

// Simple cubic interpolation 
let splineInterp (x1: float) (y1: float) (x2: float) (y2: float) (x: float) : float =
    linearInterp x1 y1 x2 y2 x