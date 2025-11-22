module NumberSystem

open System


// TYPE DEFINITIONS


type Number =
    | Integer of int64
    | Float of float
    | Rational of int64 * int64  // numerator, denominator
    | Complex of float * float    // real, imaginary


// HELPER FUNCTIONS FOR RATIONALS


// Greatest Common Divisor (Euclidean algorithm)
let rec private gcd a b =
    if b = 0L then abs a
    else gcd b (a % b)

// Simplify a rational number
let private simplifyRational (num: int64) (den: int64) =
    if den = 0L then
        failwith "Division by zero: denominator cannot be zero"
    let g = gcd num den
    let simplifiedNum = num / g
    let simplifiedDen = den / g
    // Keep denominator positive
    if simplifiedDen < 0L then
        (-simplifiedNum, -simplifiedDen)
    else
        (simplifiedNum, simplifiedDen)


// NUMBER CONVERSION


let toFloat = function
    | Integer i -> float i
    | Float f -> f
    | Rational (n, d) -> float n / float d
    | Complex (r, _) -> r

let toComplex = function
    | Integer i -> (float i, 0.0)
    | Float f -> (f, 0.0)
    | Rational (n, d) -> (float n / float d, 0.0)
    | Complex (r, i) -> (r, i)

// Simplify a number to its simplest form
let simplifyNumber = function
    | Rational (n, d) when d = 1L -> Integer n
    | Rational (n, d) -> 
        let (sn, sd) = simplifyRational n d
        if sd = 1L then Integer sn
        else Rational (sn, sd)
    | Complex (r, i) when abs i < 1e-10 -> Float r
    | num -> num


// ARITHMETIC OPERATIONS (Public Interface)


let add a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x + y)
    | Float x, Float y -> Float (x + y)
    | Float x, Integer y -> Float (x + float y)
    | Integer x, Float y -> Float (float x + y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2 + n2 * d1, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n + i * d, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d + n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d + f)
    | Float f, Rational (n, d) ->
        Float (f + float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 + r2, i1 + i2))
    | Complex (r, i), other | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r + r2, i + i2))

let subtract a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x - y)
    | Float x, Float y -> Float (x - y)
    | Float x, Integer y -> Float (x - float y)
    | Integer x, Float y -> Float (float x - y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2 - n2 * d1, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n - i * d, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d - n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d - f)
    | Float f, Rational (n, d) ->
        Float (f - float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 - r2, i1 - i2))
    | Complex (r, i), other ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r - r2, i - i2))
    | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r2 - r, i2 - i))

let multiply a b =
    match (a, b) with
    | Integer x, Integer y -> Integer (x * y)
    | Float x, Float y -> Float (x * y)
    | Float x, Integer y -> Float (x * float y)
    | Integer x, Float y -> Float (float x * y)
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * n2, d1 * d2))
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n * i, d))
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * n, d))
    | Rational (n, d), Float f ->
        Float (float n / float d * f)
    | Float f, Rational (n, d) ->
        Float (f * float n / float d)
    | Complex (r1, i1), Complex (r2, i2) ->
        simplifyNumber (Complex (r1 * r2 - i1 * i2, r1 * i2 + i1 * r2))
    | Complex (r, i), other | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        simplifyNumber (Complex (r * r2 - i * i2, r * i2 + i * r2))

let divide a b =
    match (a, b) with
    | Integer x, Integer y when y = 0L -> 
        failwith "Division by zero"
    | Integer x, Integer y when x % y = 0L -> 
        Integer (x / y)
    | Integer x, Integer y -> 
        simplifyNumber (Rational (x, y))
    | Float x, Float y when y = 0.0 -> 
        failwith "Division by zero"
    | Float x, Float y -> Float (x / y)
    | Float x, Integer y when y = 0L ->
        failwith "Division by zero"
    | Float x, Integer y -> Float (x / float y)
    | Integer x, Float y when y = 0.0 ->
        failwith "Division by zero"
    | Integer x, Float y -> Float (float x / y)
    | Rational (n1, d1), Rational (n2, d2) when n2 = 0L ->
        failwith "Division by zero"
    | Rational (n1, d1), Rational (n2, d2) ->
        simplifyNumber (Rational (n1 * d2, d1 * n2))
    | Rational (n, d), Integer i when i = 0L ->
        failwith "Division by zero"
    | Rational (n, d), Integer i ->
        simplifyNumber (Rational (n, d * i))
    | Integer i, Rational (n, d) when n = 0L ->
        failwith "Division by zero"
    | Integer i, Rational (n, d) ->
        simplifyNumber (Rational (i * d, n))
    | Rational (n, d), Float f when f = 0.0 ->
        failwith "Division by zero"
    | Rational (n, d), Float f ->
        Float (float n / float d / f)
    | Float f, Rational (n, d) when n = 0L ->
        failwith "Division by zero"
    | Float f, Rational (n, d) ->
        Float (f * float d / float n)
    | Complex (r1, i1), Complex (r2, i2) when r2 = 0.0 && i2 = 0.0 ->
        failwith "Division by zero"
    | Complex (r1, i1), Complex (r2, i2) ->
        let denominator = r2 * r2 + i2 * i2
        simplifyNumber (Complex ((r1 * r2 + i1 * i2) / denominator, 
                                 (i1 * r2 - r1 * i2) / denominator))
    | Complex (r, i), other ->
        let (r2, i2) = toComplex other
        if r2 = 0.0 && i2 = 0.0 then
            failwith "Division by zero"
        let denominator = r2 * r2 + i2 * i2
        simplifyNumber (Complex ((r * r2 + i * i2) / denominator, 
                                 (i * r2 - r * i2) / denominator))
    | other, Complex (r, i) ->
        let (r2, i2) = toComplex other
        if r = 0.0 && i = 0.0 then
            failwith "Division by zero"
        let denominator = r * r + i * i
        simplifyNumber (Complex ((r2 * r + i2 * i) / denominator, 
                                 (i2 * r - r2 * i) / denominator))

let modulo a b =
    match (a, b) with
    | Integer x, Integer y when y = 0L ->
        failwith "Modulo by zero"
    | Integer x, Integer y -> Integer (x % y)
    | _ -> 
        let fa = toFloat a
        let fb = toFloat b
        if fb = 0.0 then failwith "Modulo by zero"
        Float (fa % fb)

let power a b =
    match (a, b) with
    | Integer x, Integer y when y >= 0L ->
        Integer (pown x (int y))
    | _ ->
        let fa = toFloat a
        let fb = toFloat b
        Float (fa ** fb)

let negate = function
    | Integer i -> Integer (-i)
    | Float f -> Float (-f)
    | Rational (n, d) -> Rational (-n, d)
    | Complex (r, i) -> Complex (-r, -i)


// STRING REPRESENTATION


let toString = function
    | Integer i -> string i
    | Float f -> string f
    | Rational (n, d) when d = 1L -> string n
    | Rational (n, d) -> sprintf "%d/%d" n d
    | Complex (r, 0.0) -> string r
    | Complex (0.0, i) -> sprintf "%gi" i
    | Complex (r, i) when i >= 0.0 -> sprintf "%g+%gi" r i
    | Complex (r, i) -> sprintf "%g%gi" r i



// FILE 2: Lexer.fs

// This module handles tokenization (string -> tokens)
// It DEPENDS on NumberSystem for the Number type

module Lexer

open System
open NumberSystem  // <-- Import the NumberSystem module!


// TOKEN TYPES


type Terminal = 
    | Add | Sub | Mul | Div | Mod | Pow
    | Lpar | Rpar 
    | Num of Number              // <-- Uses Number from NumberSystem!
    | Func of string
    | Ident of string
    | Assign
    | Semicolon


// HELPER FUNCTIONS


let private str2lst s = [for c in s -> c]
let private isblank c = Char.IsWhiteSpace c
let private isdigit c = Char.IsDigit c
let private isLetter c = Char.IsLetter c
let private isLetterOrDigit c = Char.IsLetterOrDigit c
let private intVal (c:char) = int c - int '0'

// Exception
let lexError = Exception("Lexer error")


// NUMBER SCANNERS


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
// Returns Number type from NumberSystem module
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
        let result = mantissa * (10.0 ** expVal)
        
        // Create appropriate Number type
        let number = 
            if fracPart = 0.0 && expVal = 0.0 then
                Integer (int64 result)  // It's an integer!
            else
                Float result            // It's a float
                
        (afterExp, number)
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


// MAIN LEXER FUNCTION (Public Interface)


let tokenize input = 
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
            let (remaining, number) = scNum(input)
            Num number :: scan remaining  // <-- Returns Number type!
        | c :: tail when isLetter c -> 
            let (remaining, name) = scIdent(tail, string c)
            match recognizeFunction name with
            | Some funcToken -> funcToken :: scan remaining
            | None -> Ident name :: scan remaining
        | _ -> raise lexError
    scan (str2lst input)

// Helper function to print tokens (for debugging)
let printTokens (tokens: Terminal list) : unit =
    tokens |> List.iter (fun t -> 
        match t with
        | Num n -> printf "Num(%s) " (NumberSystem.toString n)
        | _ -> printf "%A " t
    )
    printfn "EOL"



// FILE 3: Program.fs (Main file showing how modules interact)


module Program

open System
open NumberSystem  // <-- Import NumberSystem
open Lexer         // <-- Import Lexer (which already imported NumberSystem)

// Example: Using both modules together
[<EntryPoint>]
let main argv =
    printfn "=== DEMONSTRATION: Module Interaction ===\n"
    
    // -----------------------------------------------
    // Test 1: Using NumberSystem directly
    // -----------------------------------------------
    printfn "Test 1: Direct NumberSystem usage"
    let num1 = Integer 5L
    let num2 = Integer 3L
    let sum = NumberSystem.add num1 num2
    printfn "  %s + %s = %s" 
        (NumberSystem.toString num1) 
        (NumberSystem.toString num2) 
        (NumberSystem.toString sum)
    
    // Test with rationals
    let rat1 = Rational (1L, 2L)
    let rat2 = Rational (1L, 3L)
    let ratSum = NumberSystem.add rat1 rat2
    printfn "  %s + %s = %s\n" 
        (NumberSystem.toString rat1) 
        (NumberSystem.toString rat2) 
        (NumberSystem.toString ratSum)
    
    // -----------------------------------------------
    // Test 2: Using Lexer (which uses NumberSystem)
    // -----------------------------------------------
    printfn "Test 2: Lexer tokenization (uses NumberSystem internally)"
    let input1 = "3 + 4 * 5"
    printfn "  Input: %s" input1
    let tokens1 = Lexer.tokenize input1
    printf "  Tokens: "
    Lexer.printTokens tokens1
    
    let input2 = "1.5 + 2.7"
    printfn "\n  Input: %s" input2
    let tokens2 = Lexer.tokenize input2
    printf "  Tokens: "
    Lexer.printTokens tokens2
    
    // -----------------------------------------------
    // Test 3: Pattern matching on tokens to extract Numbers
    // -----------------------------------------------
    printfn "\nTest 3: Extracting and operating on Numbers from tokens"
    let extractNumbers tokens =
        tokens 
        |> List.choose (fun t -> 
            match t with 
            | Num n -> Some n 
            | _ -> None)
    
    let numbers = extractNumbers tokens2
    match numbers with
    | [n1; n2] ->
        let result = NumberSystem.add n1 n2
        printfn "  Extracted numbers: %s and %s" 
            (NumberSystem.toString n1) 
            (NumberSystem.toString n2)
        printfn "  Sum: %s" (NumberSystem.toString result)
    | _ -> printfn "  Unexpected number of numeric tokens"
    
    printfn "\n=== Module interaction successful! ===\n"
    0