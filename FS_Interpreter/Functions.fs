module Functions

open System
open NumberSystem

let applyFunction (name: string) (arg: Number) : Number =
    let argFloat = NumberSystem.toFloat arg
    
    match name with
    // Trigonometric functions (angles in radians)
    | "sin" -> Float (Math.Sin(argFloat))
    | "cos" -> Float (Math.Cos(argFloat))
    | "tan" -> Float (Math.Tan(argFloat))
    | "asin" -> Float (Math.Asin(argFloat))
    | "acos" -> Float (Math.Acos(argFloat))
    | "atan" -> Float (Math.Atan(argFloat))
    // Exponential and logarithmic
    | "exp" -> Float (Math.Exp(argFloat))
    | "log" -> Float (Math.Log10(argFloat))  // Base 10
    | "ln" -> Float (Math.Log(argFloat))     // Natural logarithm
    // Utility functions
    | "sqrt" -> 
        if argFloat < 0.0 then 
            raise (System.Exception("Cannot take square root of negative number"))
        Float (Math.Sqrt(argFloat))
    | "abs" -> Float (Math.Abs(argFloat))
    | "floor" -> Float (Math.Floor(argFloat))
    | "ceil" -> Float (Math.Ceiling(argFloat))
    | "round" -> Float (Math.Round(argFloat))
    | _ -> raise (System.Exception($"Unknown function: {name}"))