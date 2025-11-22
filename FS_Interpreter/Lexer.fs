module Lexer

open System
open NumberSystem

type Terminal = 
    | Add | Sub | Mul | Div | Mod | Pow
    | Lpar | Rpar 
    | Num of Number
    | Func of string
    | Ident of string
    | Assign
    | Semicolon

// Helper functions
let private str2lst s = [for c in s -> c]
let private isblank c = System.Char.IsWhiteSpace c
let private isdigit c = System.Char.IsDigit c
let private isLetter c = System.Char.IsLetter c
let private isLetterOrDigit c = System.Char.IsLetterOrDigit c
let private intVal (c:char) = int c - int '0'

// Exception
let lexError = System.Exception("Lexer error")

// Recursive digit scanner 
let rec private scInt(iStr, iVal:float) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10.0*iVal+(float(intVal c)))
    | _ -> (iStr, iVal)

// Recursive fraction scanner
let rec private scFrac(iStr, fracVal, divisor) =
    match iStr with
    | c :: tail when isdigit c -> 
        let newFrac = fracVal + (float (intVal c)) / divisor
        scFrac(tail, newFrac, divisor * 10.0)
    | _ -> (iStr, fracVal)

// Exponent scanner for exponential notation (e.g., 1.5E3)
let private scExponent(iStr) =
    match iStr with
    | [] -> ([], 0.0)
    | ('E' | 'e') :: tail ->
        match tail with
        | '+' :: digitTail ->
            let (remaining, expVal) = scInt(digitTail, 0.0)
            (remaining, expVal)
        | '-' :: digitTail ->
            let (remaining, expVal) = scInt(digitTail, 0.0)
            (remaining, -expVal)
        | c :: _ when isdigit c ->
            let (remaining, expVal) = scInt(tail, 0.0)
            (remaining, expVal)
        | _ -> raise lexError
    | _ -> (iStr, 0.0)

// Number scanner - handles integers, floats, and exponential notation
let private scNum(iStr) =
    match iStr with
    | c :: tail when isdigit c -> 
        let (afterInt, intPart) = scInt(tail, float (intVal c))
        let (afterFrac, fracPart) = 
            match afterInt with
            | '.' :: fracTail -> scFrac(fracTail, 0.0, 10.0)
            | _ -> (afterInt, 0.0)
        let (afterExp, expVal) = scExponent(afterFrac)
        let mantissa = intPart + fracPart
        let result = mantissa * (10.0 ** expVal)  // Calculate result
        let number = 
            if fracPart = 0.0 && expVal = 0.0 then
                Integer (int64 result)  // Pure integer
            else
                Float result            // Float or exponential
        (afterExp, number)  // Return tuple
    | _ -> raise lexError

// Scan an identifier or function name
let rec private scIdent(iStr, acc: string) =
    match iStr with
    | c :: tail when isLetterOrDigit c || c = '_' -> 
        scIdent(tail, acc + string c)
    | _ -> (iStr, acc)

// Check if a name is a recognized built-in function
let private recognizeFunction (name: string) : Terminal option =
    match name.ToLower() with
    | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" 
    | "exp" | "log" | "ln" | "sqrt" | "abs" 
    | "floor" | "ceil" | "round" -> Some (Func (name.ToLower()))
    | _ -> None

// Lexer function - converts input string to list of terminals
let lexer input = 
    let rec scan input =
        match input with
        | [] -> []
        | '+'::tail -> Add :: scan tail
        | '-'::tail -> Sub :: scan tail
        | '*'::tail -> Mul :: scan tail
        | '/'::tail -> Div :: scan tail
        | '%'::tail -> Mod :: scan tail
        | '^'::tail -> Pow :: scan tail
        | '('::tail -> Lpar :: scan tail
        | ')'::tail -> Rpar :: scan tail
        | '='::tail -> Assign :: scan tail
        | ';'::tail -> Semicolon :: scan tail
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> 
            let (remaining, numVal) = scNum(input)
            Num numVal :: scan remaining
        | c :: tail when isLetter c -> 
            let (remaining, name) = scIdent(tail, string c)
            match recognizeFunction name with
            | Some funcToken -> funcToken :: scan remaining
            | None -> Ident name :: scan remaining
        | _ -> raise lexError
    scan (str2lst input)