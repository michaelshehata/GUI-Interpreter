module public Lexer

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
    // INT 3 LOOPS
    | For
    | To
    | Step
    | Do
    | End
    | LBrace
    | RBrace
    | Comma
    // STATIC TYPING
    | IntType
    | FloatType
    | ComplexType
    | RatType




// Helper functions
let private str2lst s = [for c in s -> c]
let private isblank c = System.Char.IsWhiteSpace c
let private isdigit c = System.Char.IsDigit c
let private isLetter c = System.Char.IsLetter c
let private isLetterOrDigit c = System.Char.IsLetterOrDigit c
let private intVal (c:char) = int c - int '0'

// Exception
let lexError = System.Exception("Lexer error")


// Purpose: Scan consecutive digits and build the integer part of a number.
// Arguments: iStr - remaining char list, iVal - curret accumulated value
// Returns: (remainingChars, parsedValue) as char list, float.
let rec private scInt(iStr, iVal:float) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10.0*iVal+(float(intVal c)))
    | _ -> (iStr, iVal)


// Purpose: Scan fractional digits after '.' and accumulate decimal value.
// Arguments: iStr - remaining chars, fracVal - accumulated fraction, divisor - current divisor.
// Returns: (remainingChars, fracValue) as char list, float.
let rec private scFrac(iStr, fracVal, divisor) =
    match iStr with
    | c :: tail when isdigit c -> 
        let newFrac = fracVal + (float (intVal c)) / divisor
        scFrac(tail, newFrac, divisor * 10.0)
    | _ -> (iStr, fracVal)


// Purpose: Parse optional exponent part 
// Arguments: iStr - remaining char list starting at exponent marker or next token.
// Returns: (remainingChars, expValue) as char list, float.
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


// Purpose: Parse an integer/float/exponential number (optionally an 'i')
// Arguments: iStr - remaining char list list starting with a digit
// Returns: (remainingChars, Number) as (char list, Number).
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
        
        // NEW: Check for 'i' 
        match afterExp with
        | 'i' :: rest ->
            // imaginary: 3i = 0+3i
            (rest, Complex(0.0, result))
        | _ ->
            let number = 
                if fracPart = 0.0 && expVal = 0.0 then
                    Integer (int64 result)
                else
                    Float result
            (afterExp, number)
    | _ -> raise lexError


// Purpose: Scan an identifier consisting of letters/digits/underscore.
// Arguments: iStr - remaining chars, acc - current id string.
// Returns: (remainingChars, name) as char list, string.
let rec private scIdent(iStr, acc: string) =
    match iStr with
    | c :: tail when isLetterOrDigit c || c = '_' -> 
        scIdent(tail, acc + string c)
    | _ -> (iStr, acc)


// Purpose: Map reserved keywords and built-in function names to Terminal tokens.
// Arguments: name - identifier string to classify.
// Returns: Some Terminal or None.
let private recognizeFunction (name: string) : Terminal option =
    match name.ToLower() with
    | "for" -> Some For | "to" -> Some To | "step" -> Some Step
    | "do" -> Some Do | "end" -> Some End
    //Static typing keywords
    | "int" -> Some IntType
    | "float" -> Some FloatType
    | "rat" -> Some RatType
    | "complex" -> Some ComplexType
    // Functions
    | "sin" | "cos" | "tan" | "asin" | "acos" | "atan" 
    | "exp" | "log" | "ln" | "sqrt" | "abs" 
    | "floor" | "ceil" | "round" 
    | "plot" | "interpolation" -> Some (Func (name.ToLower())) 
    | _ -> None

// Lexer function - converts input string to list of terminals
// Purpose: Convert a raw input string into a list of Terminal tokens.
// Arguments: input - string entered by the user.
// Returns: Terminal list representing the token stream.
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
        | '{'::tail -> LBrace :: scan tail
        | '}'::tail -> RBrace :: scan tail
        | '='::tail -> Assign :: scan tail
        | ';'::tail -> Semicolon :: scan tail
        | ','::tail -> Comma :: scan tail
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