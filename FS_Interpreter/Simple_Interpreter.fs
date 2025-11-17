// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

// Group 21 Advanced Programming
// Michael Shehata, Ali Jamjoum, Luke Wilson

open System

// Terminal types for lexer
type terminal = 
    | Add | Sub | Mul | Div | Mod | Pow
    | Lpar | Rpar 
    | Num of float
    | Func of string  // Built-in functions (sin, cos, etc.)
    | Ident of string // Variable names (for future use)
    | Assign          // Assignment operator (for future use)
    | Semicolon       // Statement terminator (for future use)

type SymbolTable = Map<string, float>
let mutable symbolTable : SymbolTable = Map.empty

type ParseTree =
    | Node of string * ParseTree list
    | Leaf of string


// Helper functions
let str2lst s = [for c in s -> c]
let isblank c = System.Char.IsWhiteSpace c
let isdigit c = System.Char.IsDigit c
let isLetter c = System.Char.IsLetter c
let isLetterOrDigit c = System.Char.IsLetterOrDigit c
let intVal (c:char) = int c - int '0'

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

// Exponent scanner for exponential notation (e.g., 1.5E3)
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

// Number scanner - handles integers, floats, and exponential notation
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

// Scan an identifier or function name
let rec scIdent(iStr, acc: string) =
    match iStr with
    | c :: tail when isLetterOrDigit c || c = '_' -> 
        scIdent(tail, acc + string c)
    | _ -> (iStr, acc)

// Check if a name is a recognized built-in function
let recognizeFunction (name: string) : terminal option =
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

// Parser function - validates syntax without evaluation
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
        | Ident name :: tail -> tail
        | Func name :: Lpar :: tail -> 
            match E tail with 
            | Rpar :: tail -> tail
            | _ -> raise parseError
        | Lpar :: tail -> 
            match E tail with 
            | Rpar :: tail -> tail
            | _ -> raise parseError
        | _ -> raise parseError
    let remaining = E tList
    match remaining with
    | [] -> []
    | _ -> raise parseError

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

// Parse tree builder
let parseExpTree tList =    
    let rec E tList = 
        let (remaining,tTree) = T tList
        let (restEopt,eoptTree) = Eopt remaining
        (restEopt, Node("<exp>",[tTree;eoptTree]))

    and Eopt tList = // parse any extra "+ value" or "- value"
        match tList with
        | Add :: tail ->
            let (remaining,tTree) = T tail
            let (restEopt, eoptTree) = Eopt remaining
            (restEopt, Node("<Eopt>", [Leaf "+" ; tTree ; eoptTree]))
        | Sub :: tail ->
            let (remaining, tTree) = T tail
            let (restEopt, eoptTree) = Eopt remaining
            (restEopt, Node("<Eopt>", [Leaf "-" ; tTree ; eoptTree]))
        | _ -> // if no + or - value, end expression
            (tList, Node("<Eopt>",[Leaf "ε"]))

    and T tList = 
        let (restP, pTree) =  P tList
        let (restTopt, toptTree) = Topt restP
        (restTopt, Node("<term>",[pTree; toptTree]))
    
    and Topt tList = 
        match tList with
        | Mul :: tail ->
            let(restP, pTree) = P tail
            let (restTopt, toptTree) = Topt restP
            (restTopt, Node("<Topt>",[Leaf "*"; pTree ; toptTree]))
        | Div :: tail ->
            let(restP, pTree) = P tail
            let (restTopt, toptTree) = Topt restP
            (restTopt, Node("<Topt>",[Leaf "/"; pTree ; toptTree]))
        | Mod :: tail ->
            let(restP, pTree) = P tail
            let (restTopt, toptTree) = Topt restP
            (restTopt, Node("<Topt>",[Leaf "%"; pTree ; toptTree]))
        | _ ->
            (tList, Node("<Topt>", [Leaf "ε"]))
    
    and P tList =
        let(restU, uTree) = U tList
        let (restPopt, poptTree) = Popt restU
        (restPopt, Node("<P>", [uTree;poptTree]))

    and Popt tList = 
        match tList with
        | Pow :: tail ->
            let (restP, pTree) = P tail
            (restP, Node("<Popt>", [Leaf "^"; pTree]))
        | _ ->
            (tList, Node("<Popt>", [Leaf "ε"]))

    and U tList =
        match tList with
        | Sub :: tail ->
            let (restU, uTree) = U tail
            (restU, Node("<U>", [Leaf "-"; uTree]))
        | _ ->
            NR tList
    
    and NR tList = 
        match tList with
        | Num value :: tail ->
            (tail, Node("<NR>", [Leaf (string value)]))
        | Ident name :: tail ->
            (tail, Node("<NR>", [Leaf name]))
        | Func name :: Lpar :: tail ->
            let (afterExp, eTree) = E tail
            match afterExp with
            | Rpar :: rest ->
                (rest, Node("<NR>", [Leaf name; Leaf "("; eTree; Leaf ")"]))
            | _ ->
                raise parseError
        | Lpar :: tail ->
            let (restE, eTree) = E tail
            match restE with
            | Rpar :: rest ->
                (rest, Node("<NR>", [Leaf "("; eTree; Leaf ")"])) 
            | _ -> raise parseError
        | _ -> raise parseError
    
    let (remaining, tree) = E tList
    match remaining with
    | [] -> (remaining, tree)
    | _ -> raise  parseError
let parseStatementTree tList = 
    match tList with
    | Ident name :: Assign :: tail ->
        let (remaining, expTree) = parseExpTree tail
        match remaining with
        | [] ->
            (remaining, Node("<statement>", [Leaf name; Leaf "="; expTree]))
        | _ -> raise parseError
    | _ ->
        let (remaining, expTree) = parseExpTree tList
        match remaining with
        | [] ->
            (remaining, Node("<statement>", [expTree]))
        | _ -> raise parseError

open System.Text

// New addition for turning tree into string
let parseTreetoString (tree:ParseTree) : string = 
    let sb = StringBuilder()
    
    let rec print indent t =
        let ind = String.replicate indent " "
        match t with
        | Leaf  text ->
            sb.AppendLine(sprintf "%s%s" ind text) |> ignore
        | Node (label,children) ->
            sb.AppendLine(sprintf "%s%s" ind label) |> ignore
            children |> List.iter (print(indent + 1))
    print 0 tree
    sb.ToString()
// CHANGE 1: Statement parser - handles both assignments and expressions
let parseStatement tList = 
    match tList with
    | Ident name :: Assign :: tail ->
        // This is an assignment: variable = expression
        let (remaining, value) = parseNeval tail
        symbolTable <- symbolTable.Add(name, value)
        (remaining, value)
    | _ -> 
        // Regular expression (no assignment)
        parseNeval tList

// Function to print list of terminals (for debugging)
let rec printTList (lst:list<terminal>) : list<string> = 
    match lst with
    | head::tail -> 
        Console.Write("{0} ", head.ToString())
        printTList tail
    | [] -> 
        Console.Write("EOL\n")
        []

// Function to get input from console
let getInputString() : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Testing suite module
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
    
    // Comprehensive test cases
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
        
        // Exponential notation
        { Expression = "1.5E3"; Expected = 1500.0; Description = "Exponential notation (E)" }
        { Expression = "2.5e-2"; Expected = 0.025; Description = "Exponential notation (e-)" }
        { Expression = "1E+2"; Expected = 100.0; Description = "Exponential notation (E+)" }

        // Built-in functions
        { Expression = "sin(0)"; Expected = 0.0; Description = "sin(0)" }
        { Expression = "cos(0)"; Expected = 1.0; Description = "cos(0)" }
        { Expression = "sqrt(16)"; Expected = 4.0; Description = "sqrt(16)" }
        { Expression = "abs(-5)"; Expected = 5.0; Description = "abs(-5)" }
        { Expression = "exp(0)"; Expected = 1.0; Description = "exp(0)" }
        { Expression = "ln(1)"; Expected = 0.0; Description = "ln(1)" }
        { Expression = "floor(3.7)"; Expected = 3.0; Description = "floor(3.7)" }
        { Expression = "ceil(3.2)"; Expected = 4.0; Description = "ceil(3.2)" }
        { Expression = "round(3.5)"; Expected = 4.0; Description = "round(3.5)" }
        
        // Function composition
        { Expression = "sin(0) + cos(0)"; Expected = 1.0; Description = "Function in expression" }
        { Expression = "sqrt(4) * 3"; Expected = 6.0; Description = "Function result in multiplication" }
        { Expression = "abs(-2 + -3)"; Expected = 5.0; Description = "Function with expression argument" }
    ]
    
    let runTest (testCase: TestCase) : TestResult =
        try
            let tokens = lexer testCase.Expression
            let (_, result) = parseNeval tokens
            let passed = abs(result - testCase.Expected) < 0.0001
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

// Public interface for GUI integration
module public GUIInterpret =
    
    let interpret(input:string) : string =
        try
            let oList = lexer input
            let _ = printTList oList
            let (_, result) = parseStatement oList
            result.ToString()
        with
        | ex -> raise (System.Exception(ex.Message))
    
    let evaluateExpression(expression:string, xValue:float) : float =
        try
            // Store x value in symbol table
            symbolTable <- symbolTable.Add("x", xValue)
            
            // Lex and evaluate the expression
            let oList = lexer expression
            let (_, result) = parseNeval oList
            result
        with
        | ex -> raise (System.Exception($"Error evaluating expression: {ex.Message}"))
    
    let getParseTreeString (input:string) :string =
        try
            let tokens = lexer input
            let (_, tree) = parseStatementTree tokens
            parseTreetoString tree
        with
        | ex -> raise (System.Exception($"Error generating parse tree: {ex.Message}"))

// Entry point
[<EntryPoint>]
let main argv =
    Console.WriteLine("Simple Interpreter - Starting Tests...")
    Console.WriteLine(String.replicate 50 "=")
    
    // Run automated tests
    let (passed, failed) = Testing.runAllTests()
    
    // Provide feedback
    if failed = 0 then
        Console.ForegroundColor <- ConsoleColor.Green
        Console.WriteLine("\n✓ ALL TESTS PASSED!\n")
        Console.ResetColor()
    else
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine($"\n✗ WARNING: {failed} test(s) FAILED.\n")
        Console.ResetColor()
    
    // Interactive mode
    Console.WriteLine(String.replicate 50 "=")
    Console.WriteLine("Interactive Mode - Enter expressions below:")
    Console.WriteLine(String.replicate 50 "=")
    
    let input = getInputString()
    
    try
        let oList = lexer input
        let _ = printTList oList
        let Out = parseStatement oList
        Console.WriteLine("Result = {0}", snd Out)
        0
    with
    | ex ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("Error: {0}", ex.Message)
        Console.ResetColor()
        1

// Grammar in BNF:
// CHANGE 3: Updated grammar to include statement rule
// <Statement> ::= <Ident> "=" <E> | <E>
// <E>        ::= <T> <Eopt>
// <Eopt>     ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>        ::= <P> <Topt>
// <Topt>     ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>        ::= <U> <Popt>
// <Popt>     ::= "^" <P> | <empty>
// <U>        ::= "-" <U> | <NR>
// <NR>       ::= "Num" <value> | "Ident" <name> | <Func> "(" <E> ")" | "(" <E> ")"