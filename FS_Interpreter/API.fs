module public API

open System
open NumberSystem
open Lexer
open Parser
open Evaluator
open SymbolTable
open AST
open PlotBuffer

// Function to print list of terminals (for debugging)
let rec printTList (lst: Terminal list) : unit = 
    match lst with
    | head::tail -> 
        Console.Write("{0} ", head.ToString())
        printTList tail
    | [] -> 
        Console.Write("EOL\n")

// Main interpreter function
let interpret (input: string) : string =
    try
        let oList = lexer input
        let _ = printTList oList
        let (_, result) = parseStatement oList
        NumberSystem.toString result
    with
    | ex -> raise (System.Exception(ex.Message))

// Evaluate expression with x value (for plotting)
let evaluateExpression (expression: string, xValue: float) : float =
    try
        // Store x value in symbol table
        SymbolTable.current <- SymbolTable.add "x" (Float xValue) SymbolTable.current
        
        // Lex and evaluate the expression
        let oList = lexer expression
        let (_, result) = parseNeval oList
        NumberSystem.toFloat result
    with
    | ex -> raise (System.Exception($"Error evaluating expression: {ex.Message}"))

// Get parse tree as string
let getParseTreeString (input: string) : string =
    try
        let tokens = lexer input
        let (_, tree) = parseStatementTree tokens
        AST.parseTreeToString tree
    with
    | ex -> raise (System.Exception($"Error generating parse tree: {ex.Message}"))

// Clear all variables
let clearVariables () : unit =
    SymbolTable.current <- SymbolTable.empty

// Get all variables as formatted string
let getVariables () : string =
    if Map.isEmpty SymbolTable.current then
        "No variables defined"
    else
        SymbolTable.current
        |> Map.toList
        |> List.map (fun (name, value) -> 
            sprintf "%s = %s" name (NumberSystem.toString value))
        |> String.concat "\n"

// Validate syntax without executing
let validateSyntax (input: string) : bool =
    try
        let tokens = lexer input
        let _ = Parser.parser tokens
        true
    with
    | ex -> raise (System.Exception($"Syntax error: {ex.Message}"))

// PLOTS FOR INT3
// Get plot points
let getPlotPoints () : (float * float) list =
    PlotBuffer.getPoints() 
    |> List.map (fun p -> (p.X, p.Y))

// Clear plot points
let clearPlotPoints () : unit =
    PlotBuffer.clear()

// Execute and plot a function
let plotFunction (expression: string) (xMin: float) (xMax: float) (step: float) : unit =
    PlotBuffer.clear()
    
    let mutable x = xMin
    while x <= xMax do
        try
            SymbolTable.current <- SymbolTable.add "x" (Float x) SymbolTable.current
            let tokens = lexer expression
            let (_, result) = parseNeval tokens
            let y = NumberSystem.toFloat result
            PlotBuffer.addPoint x y
        with
        | ex -> () // Skip points that cause errors
        
        x <- x + step
let getInterpolationMode () : int =
    match PlotBuffer.getInterpolation() with
    | PlotBuffer.Linear -> 0
    | PlotBuffer.Spline -> 1

// Get function help text
let getFunctionHelp () : string =
    """
Available Built-in Functions:
=============================

Trigonometric (radians):
  sin(x), cos(x), tan(x)
  asin(x), acos(x), atan(x)

Exponential & Logarithmic:
  exp(x)  - e^x
  ln(x)   - natural logarithm
  log(x)  - base-10 logarithm
  sqrt(x) - square root

Utility:
  abs(x)   - absolute value
  floor(x) - round down
  ceil(x)  - round up
  round(x) - round to nearest integer

Examples:
  sin(0)        → 0
  sqrt(16)      → 4
  abs(-5)       → 5
  exp(1)        → 2.71828...
"""

// Get syntax help text
let getSyntaxHelp () : string =
    """
Syntax Guide:
=============

Operators (BODMAS order):
  ^   - Power (right associative)
  * / % - Multiply, Divide, Modulo
  + -   - Add, Subtract
  -x    - Unary minus (negation)

Number Types:
  123       - Integer
  3.14      - Float
  1.5E3     - Exponential notation (1500)

Variables:
  x = 10;          - Assignment
  y = x + 5;       - Use in expressions
  z = 2*x + 3*y;   - Multiple variables

Functions:
  y = sin(x) + 2;  - Built-in functions
  result = sqrt(x^2 + y^2);

Parentheses:
  (2 + 3) * 4      - Override operator precedence

Examples:
  3 + 4 * 5        → 23 (not 35)
  (3 + 4) * 5      → 35
  2^3^2            → 512 (right associative)
  -5 + 3           → -2
"""