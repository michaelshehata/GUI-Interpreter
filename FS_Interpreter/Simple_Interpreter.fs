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
    | Sin | Cos | Tan | Log | Exp
    | Ident of string
    | Assign
    | Semicolon

//Helper functions
let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let intVal (c:char) = (int)((int)c - (int)'0')

// Exceptions
let parseError = System.Exception("Parser error")
let lexError = System.Exception("Lexer error")
let runtimeError = System.Exception("Runtime error")

// Recursive digit scanner 
let rec scInt(iStr, iVal:float) = 
    match iStr with
    | c :: tail when isdigit c -> scInt(tail, 10.0*iVal+(float(intVal c)))
    | _ -> (iStr, iVal)

// Recursive fraction scanner
let rec scFrac(iStr, fracVal, divisor) =
    match iStr with
    | c :: tail when isdigit c -> 
        let newFrac = fracVal + (float (intVal c)) / divisor
        scFrac(tail, newFrac, divisor * 10.0)
    | _ -> (iStr, fracVal)

// Exponent scanner
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

// Number scanner
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

// Lexer function
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

// Function to get input from console
let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Parser function
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
    let remaining = E tList
    match remaining with
    | [] -> []
    |   _ -> raise parseError
    

// Parser and evaluator function
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
    let (remaining, result) = E tList
    match remaining with
    | [] -> (remaining, result)
    | _ -> raise parseError
    

// Function to print list of terminals
let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    | head::tail -> Console.Write("{0} ",head.ToString())
                    printTList tail
    | [] -> Console.Write("EOL\n")
            []

// Testing suite
module Testing =
    
    type TestCase = {
        Expression: string
        Expected: float
        Description: string
    }
    
    type TestResult = {
        Expression: string
        Expected: float
        Actual: float option
        Passed: bool
        ErrorMsg: string option
        Description: string
    }
    
    // Test cases 
    let testCases = [
        // Basic arithmetic
        { Expression = "3 + 4 * 5"; Expected = 23.0; Description = "BODMAS test" }
        { Expression = "(3 + 4) * 5"; Expected = 35.0; Description = "Parentheses priority" }
        { Expression = "10 - 3 - 2"; Expected = 5.0; Description = "Left associativity (subtraction)" }
        { Expression = "20 / 4 / 2"; Expected = 2.5; Description = "Left associativity (division)" }
        
        // Floats
        { Expression = "3.14 + 2.5"; Expected = 5.64; Description = "Float addition" }
        { Expression = "10.5 / 2"; Expected = 5.25; Description = "Float division" }
        
        // Modulo
        { Expression = "10 % 3"; Expected = 1.0; Description = "Integer modulo" }
        { Expression = "17.5 % 5"; Expected = 2.5; Description = "Float modulo" }
        
        // Power (right-associative!)
        { Expression = "2^3"; Expected = 8.0; Description = "Simple power" }
        { Expression = "2^3^2"; Expected = 512.0; Description = "Right associativity (power)" }
        { Expression = "2^10"; Expected = 1024.0; Description = "Larger power" }
        
        // Unary minus
        { Expression = "-5 + 3"; Expected = -2.0; Description = "Unary minus" }
        { Expression = "-(3 + 4)"; Expected = -7.0; Description = "Unary minus with parentheses" }
        { Expression = "--5"; Expected = 5.0; Description = "Double unary minus" }
        { Expression = "---3"; Expected = -3.0; Description = "Triple unary minus" }
        
        // Complex expressions
        { Expression = "2 + 3 * 4 - 5"; Expected = 9.0; Description = "Mixed operators" }
        { Expression = "(2 + 3) * (4 - 1)"; Expected = 15.0; Description = "Multiple parentheses" }
        { Expression = "10 / 2 + 3 * 4"; Expected = 17.0; Description = "Division and multiplication" }
        
        // Exponential notation (if implemented)
        { Expression = "1.5E3"; Expected = 1500.0; Description = "Exponential notation (E)" }
        { Expression = "2.5e-2"; Expected = 0.025; Description = "Exponential notation (e-)" }
        { Expression = "1E+2"; Expected = 100.0; Description = "Exponential notation (E+)" }
    ]
    
    let runTest (testCase: TestCase) : TestResult =
        try
            let tokens = lexer testCase.Expression
            let (_, result) = parseNeval tokens
            let passed = abs(result - testCase.Expected) < 0.0001 // Float comparison tolerance
            {
                Expression = testCase.Expression
                Expected = testCase.Expected
                Actual = Some result
                Passed = passed
                ErrorMsg = None
                Description = testCase.Description
            }
        with
        | ex ->
            {
                Expression = testCase.Expression
                Expected = testCase.Expected
                Actual = None
                Passed = false
                ErrorMsg = Some ex.Message
                Description = testCase.Description
            }
    
    let runAllTests() =
        let results = testCases |> List.map runTest
        let passed = results |> List.filter (fun r -> r.Passed) |> List.length
        let failed = results |> List.filter (fun r -> not r.Passed) |> List.length
        
        printfn "\n========================================="
        printfn "TEST RESULTS"
        printfn "========================================="
        printfn "Total: %d | Passed: %d | Failed: %d\n" (passed + failed) passed failed
        
        results |> List.iter (fun r ->
            if r.Passed then
                printfn "✓ PASS: %s" r.Description
                printfn "  Expression: %s = %.4f" r.Expression r.Expected
            else
                printfn "✗ FAIL: %s" r.Description
                printfn "  Expression: %s" r.Expression
                printfn "  Expected: %.4f" r.Expected
                match r.Actual with
                | Some actual -> printfn "  Got: %.4f" actual
                | None -> printfn "  Error: %s" (r.ErrorMsg |> Option.defaultValue "Unknown error")
            printfn ""
        )
        
        printfn "========================================="
        (passed, failed)
module public GUIInterpret =

// Function to interpret input string and return result as string

    let interpret(input:string) : string =
        let oList = lexer input
        let _ = printTList oList
        let _ = printTList (parser oList)
        let (_, result) = parseNeval oList
        result.ToString()
    
[<EntryPoint>]
let main argv  =
    Console.WriteLine("Simple Interpreter - Starting Tests...")
    Console.WriteLine("=" |> String.replicate 50)
    
    // Run tests and get results
    let (passed, failed) = Testing.runAllTests()
    
    // Provide clear feedback based on test results
    if failed = 0 then
        Console.ForegroundColor <- ConsoleColor.Green
        Console.WriteLine("\nALL TESTS PASSED!\n")
        Console.ResetColor()
    else
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine($"\nWARNING: {failed} test(s) FAILED.\n")
        Console.ResetColor()
    
    // Continue to interactive mode
    Console.WriteLine("=" |> String.replicate 50)
    Console.WriteLine("Interactive Mode - Enter expressions below:")
    Console.WriteLine("=" |> String.replicate 50)
    
    let input:string = getInputString()
    
    try
        let oList = lexer input
        let sList = printTList oList
        let pList = printTList (parser oList)
        let Out = parseNeval oList
        Console.WriteLine("Result = {0}", snd Out)
        0  // Success exit code
    with
    | ex ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("Error: {0}", ex.Message)
        Console.ResetColor()
        1  // Error exit code


// Grammar in BNF:
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <P> | <empty>
// <U>        ::= "-" <U> | <NR>
// <NR>       ::= "Num" <value> | "(" <E> ")"


