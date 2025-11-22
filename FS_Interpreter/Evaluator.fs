module Evaluator

open System
open NumberSystem
open Lexer
open SymbolTable

// Exceptions
let runtimeError = System.Exception("Runtime error")

// Parser and evaluator function - parses and computes result
let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, NumberSystem.add value tval)
        | Sub :: tail -> 
            let (tLst, tval) = T tail
            Eopt (tLst, NumberSystem.subtract value tval)
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, NumberSystem.multiply value tval)
        | Div :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, NumberSystem.divide value tval)
        | Mod :: tail -> 
            let (tLst, tval) = P tail
            Topt (tLst, NumberSystem.modulo value tval)
        | _ -> (tList, value)
    and P tList = (U >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pow :: tail -> 
            let (tLst, pval) = P tail
            (tLst, NumberSystem.power value pval)
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail -> 
            let (tLst, uval) = U tail
            (tLst, NumberSystem.negate uval)
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Ident name :: tail ->
            match SymbolTable.tryFind name SymbolTable.current with
            | Some value -> (tail, value)
            | None -> raise (System.Exception($"Variable '{name}' not defined"))
        | Func name :: Lpar :: tail ->
            let (afterExpr, argValue) = E tail
            match afterExpr with
            | Rpar :: rest ->
                let result = 
                    let argFloat = NumberSystem.toFloat argValue
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
                (rest, result)
            | _ -> raise runtimeError
        | Lpar :: tail -> 
            let (tLst, tval) = E tail
            match tLst with 
            | Rpar :: tail -> (tail, tval)
            | _ -> raise runtimeError
        | _ -> raise runtimeError
    
    let (remaining, result) = E tList
    match remaining with
    | [] -> (remaining, result)
    | _ -> raise runtimeError

// Statement parser - handles both assignments and expressions
let parseStatement tList = 
    match tList with
    | Ident name :: Assign :: tail ->
        // This is an assignment: variable = expression
        let (remaining, value) = parseNeval tail
        SymbolTable.current <- SymbolTable.add name value SymbolTable.current
        (remaining, value)
    | _ -> 
        // Regular expression (no assignment)
        parseNeval tList