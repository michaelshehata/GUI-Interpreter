module Evaluator

open System
open Parser

// Exception
let runtimeError = System.Exception("Runtime error")

// Parser and evaluator function - parses and computes result
let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, value + tval)
        | Sub :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, value * tval)
        | Div :: tail -> 
            let (tLst, tval) = P tail
            if tval = 0.0 then raise runtimeError
            Topt (tLst, value / tval)
        | Mod :: tail -> 
            let (tLst, tval) = P tail
            if tval = 0.0 then raise runtimeError
            Topt (tLst, value % tval)
        | _ -> (tList, value)
    and P tList = (U >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pow :: tail -> 
            let (tLst, pval) = P tail
            (tLst, value ** pval)
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail -> 
            let (tLst, uval) = U tail
            (tLst, -uval)
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Ident name :: tail ->
            match symbolTable.TryFind name with
            | Some value -> (tail, value)
            | None -> raise (System.Exception($"Variable '{name}' not defined"))
        | Func name :: Lpar :: tail ->
            let (afterExpr, argValue) = E tail
            match afterExpr with
            | Rpar :: rest ->
                let result = 
                    match name with
                    // Trigonometric functions (angles in radians)
                    | "sin" -> Math.Sin(argValue)
                    | "cos" -> Math.Cos(argValue)
                    | "tan" -> Math.Tan(argValue)
                    | "asin" -> Math.Asin(argValue)
                    | "acos" -> Math.Acos(argValue)
                    | "atan" -> Math.Atan(argValue)
                    // Exponential and logarithmic
                    | "exp" -> Math.Exp(argValue)
                    | "log" -> Math.Log10(argValue)  // Base 10
                    | "ln" -> Math.Log(argValue)     // Natural logarithm
                    // Utility functions
                    | "sqrt" -> 
                        if argValue < 0.0 then 
                            raise (System.Exception("Cannot take square root of negative number"))
                        Math.Sqrt(argValue)
                    | "abs" -> Math.Abs(argValue)
                    | "floor" -> Math.Floor(argValue)
                    | "ceil" -> Math.Ceiling(argValue)
                    | "round" -> Math.Round(argValue)
                    | _ -> raise (System.Exception($"Unknown function: {name}"))
                (rest, result)
            | _ -> raise parseError
        | Lpar :: tail -> 
            let (tLst, tval) = E tail
            match tLst with 
            | Rpar :: tail -> (tail, tval)
            | _ -> raise parseError
        | _ -> raise parseError
    
    let (remaining, result) = E tList
    match remaining with
    | [] -> (remaining, result)
    | _ -> raise parseError