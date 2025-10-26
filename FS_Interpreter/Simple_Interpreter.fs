// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

// Group 21 Advanced Programming
// Michael Shehata, Ali Jamjoum , Luke Wilson

open System

type terminal = 
    | Add | Sub | Mul | Div | Mod | Pow
    | Lpar | Rpar 
    | Num of float

let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let intVal (c:char) = (int)((int)c - (int)'0')

let parseError = System.Exception("Parser error")
let lexError = System.Exception("Lexer error")
let runtimeError = System.Exception("Runtime error")

let rec scInt(iStr, iVal:float) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10.0*iVal+(float(intVal c)))
    | _ -> (iStr, iVal)

let rec scFrac(iStr, fracVal, divisor) =
    match iStr with
    | c :: tail when isdigit c -> 
        let newFrac = fracVal + (float (intVal c)) / divisor
        scFrac(tail, newFrac, divisor * 10.0)
    | _ -> (iStr, fracVal)

let scExponent(iStr) =
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

let scNum(iStr) =
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
        (afterExp, result)
    | _ -> raise lexError

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
        | c :: tail when isblank c -> scan tail
        | c :: tail when isdigit c -> let (remaining, numVal) = scNum(input)
                                      Num numVal :: scan remaining
        | _ -> raise lexError
    scan (str2lst input)

let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <P> | <empty>
// <U>        ::= "-" <U> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"

let parser tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt tList = 
        match tList with
        | Add :: tail -> (T >> Eopt) tail
        | Sub :: tail -> (T >> Eopt) tail
        | _ -> tList
    and T tList = (P >> Topt) tList
    and Topt tList =
        match tList with
        | Mul :: tail -> (P >> Topt) tail
        | Div :: tail -> (P >> Topt) tail
        | Mod :: tail -> (P >> Topt) tail
        | _ -> tList
    and P tList = (U >> Popt) tList
    and Popt tList =
        match tList with
        | Pow :: tail -> P tail
        | _ -> tList
    and U tList =
        match tList with
        | Sub :: tail -> U tail
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> tail
        | Lpar :: tail -> match E tail with 
                          | Rpar :: tail -> tail
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let parseNeval tList = 
    let rec E tList = (T >> Eopt) tList
    and Eopt (tList, value) = 
        match tList with
        | Add :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value + tval)
        | Sub :: tail -> let (tLst, tval) = T tail
                         Eopt (tLst, value - tval)
        | _ -> (tList, value)
    and T tList = (P >> Topt) tList
    and Topt (tList, value) =
        match tList with
        | Mul :: tail -> let (tLst, tval) = P tail
                         Topt (tLst, value * tval)
        | Div :: tail -> let (tLst, tval) = P tail
                         if tval = 0.0 then raise runtimeError
                         Topt (tLst, value / tval)
        | Mod :: tail -> let (tLst, tval) = P tail
                         if tval = 0.0 then raise runtimeError
                         Topt (tLst, value % tval)
        | _ -> (tList, value)
    and P tList = (U >> Popt) tList
    and Popt (tList, value) =
        match tList with
        | Pow :: tail -> let (tLst, pval) = P tail
                         (tLst, value ** pval)
        | _ -> (tList, value)
    and U tList =
        match tList with
        | Sub :: tail -> let (tLst, uval) = U tail
                         (tLst, -uval)
        | _ -> NR tList
    and NR tList =
        match tList with 
        | Num value :: tail -> (tail, value)
        | Lpar :: tail -> let (tLst, tval) = E tail
                          match tLst with 
                          | Rpar :: tail -> (tail, tval)
                          | _ -> raise parseError
        | _ -> raise parseError
    E tList

let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    | head::tail -> Console.Write("{0} ",head.ToString())
                    printTList tail
    | [] -> Console.Write("EOL\n")
            []

module public GUIInterpret =

    let interpret(input:string) : string =
        let oList = lexer input
        let _ = printTList oList
        let _ = printTList (parser oList)
        let (_, result) = parseNeval oList
        result.ToString()
    
[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter")
    let input:string = getInputString()
    let oList = lexer input
    let sList = printTList oList
    let pList = printTList (parser oList)
    let Out = parseNeval oList
    Console.WriteLine("Result = {0}", snd Out)
    0


//// TEST CASES ONLY FOR REFERENCE TO USE: CHECK INTERPRETER FUNCTIONALITY

//// Basic arithmetic
//3 + 4 * 5           // = 23 (BODMAS)
//(3 + 4) * 5         // = 35

//// Float support
//3.14 + 2.5          // = 5.64
//10.5 / 2            // = 5.25

//// Modulo
//10 % 3              // = 1
//17.5 % 5            // = 2.5

//// Power (right-associative!)
//2^3                 // = 8
//2^3^2               // = 2^(3^2) = 2^9 = 512 (NOT 64!)

//// Unary minus
//-5 + 3              // = -2
//-(3 + 4)            // = -7
//--5                 // = 5

//// Exponential notation (optional)
//1.5E3               // = 1500
//2.5e-2              // = 0.025

//// Error detection
//10 / 0              // Error: Division by zero
//5 % 0               // Error: Modulo by zero