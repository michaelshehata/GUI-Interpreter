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
        // FIXED: Use temporary variable for plotting
        let savedX = SymbolTable.tryFind "x" SymbolTable.current
        
        // Store x value as temporary
        SymbolTable.current <- SymbolTable.addTempVariable "x" (Float xValue) SymbolTable.current
        
        // Lex and evaluate the expression
        let oList = lexer expression
        let (_, result) = parseNeval oList
        let resultFloat = NumberSystem.toFloat result
        
        // Restore original x if it was user assigned, otherwise remove it
        match savedX with
        | Some value when SymbolTable.isUserAssigned "x" ->
            SymbolTable.current <- SymbolTable.add "x" value SymbolTable.current
        | _ ->
            SymbolTable.current <- SymbolTable.removeTempVariable "x" SymbolTable.current
        
        resultFloat
    with
    | ex -> raise (System.Exception($"Error evaluating expression: {ex.Message}"))

let differentiateNumeric (expression: string) (x0: float) (stepSize: float) : float =
    if stepSize <= 0.0 then
        raise (System.Exception("Step size must be positive for differentiation."))
    try
        let leftValue = evaluateExpression(expression, x0 - stepSize)
        let rightValue = evaluateExpression(expression, x0 + stepSize)
        (rightValue - leftValue) / (2.0 * stepSize)
    with
    | ex -> raise (System.Exception($"Error computing derivative: {ex.Message}"))

/// trapezium rule to approximate definite integral of f(x) from a to b
let integrateTrapezoidal (expression: string) (a: float) (b: float) (steps: int) : float =
    if steps <= 0 then
        raise (System.Exception("Number of steps must be positive for integration."))
    if a = b then 0.0
    else
        try
            // Interval swapped or not?
            let mutable startX = a
            let mutable endX = b
            let swapped =
                if startX > endX then
                    let tmp = startX
                    startX <- endX
                    endX <- tmp
                    true
                else
                    false

            let h = (endX - startX) / float steps

            
            let fStart = evaluateExpression(expression, startX)
            let fEnd = evaluateExpression(expression, endX)

            // Interior points
            let mutable sum = 0.0
            for i = 1 to steps - 1 do
                let x = startX + float i * h
                sum <- sum + evaluateExpression(expression, x)

            let area = h * ((fStart + fEnd) / 2.0 + sum)
            if swapped then -area else area
        with
        | ex -> raise (System.Exception($"Error computing integral: {ex.Message}"))

// Bisection root finder for f(x) on [a, b]
let findRootBisection (expression: string) (a: float) (b: float) (tolerance: float) (maxIterations: int) : float =
    if maxIterations <= 0 then
        raise (System.Exception("maxIterations must be positive for root finding."))
    if tolerance <= 0.0 then
        raise (System.Exception("Tolerance must be positive for root finding."))

    try
        let mutable left = a
        let mutable right = b
        let mutable fLeft = evaluateExpression(expression, left)
        let mutable fRight = evaluateExpression(expression, right)

        // need a sign change to use bisection
        if fLeft * fRight > 0.0 then
            raise (System.Exception("Bisection method requires f(a) and f(b) to have opposite signs."))

        let mutable root = (left + right) / 2.0

        let mutable i = 0
        while i < maxIterations && (right - left) / 2.0 > tolerance do
            root <- (left + right) / 2.0
            let fMid = evaluateExpression(expression, root)

            if abs fMid < tolerance then
                // if its close enough well collaps the interval
                left <- root
                right <- root
            elif fLeft * fMid < 0.0 then
                // root lives in [left, root]
                right <- root
                fRight <- fMid
            else
                // root lives in [root, right]
                left <- root
                fLeft <- fMid

            i <- i + 1

        root
    with
    | ex -> raise (System.Exception($"Error finding root: {ex.Message}"))

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
    SymbolTable.clearAll()
    SymbolTable.current <- SymbolTable.empty

// Get all variables as formatted string
let getVariables () : string =
    if Map.isEmpty SymbolTable.current then
        "No variables defined"
    else
        SymbolTable.current
        |> Map.toList
        |> List.map (fun (name, entry) -> 
            sprintf "%s = %s" name (NumberSystem.toString entry.Value))
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
    
    // FIXED: Save x if it was user-assigned
    let savedX = SymbolTable.tryFind "x" SymbolTable.current
    let wasUserAssigned = SymbolTable.isUserAssigned "x"
    
    let mutable x = xMin
    while x <= xMax do
        try
            // Use temporary variable
            SymbolTable.current <- SymbolTable.addTempVariable "x" (Float x) SymbolTable.current
            let tokens = lexer expression
            let (_, result) = parseNeval tokens
            let y = NumberSystem.toFloat result
            PlotBuffer.addPoint x y
        with
        | ex -> () // Skip points that cause errors
        
        x <- x + step
    
    // FIXED: Restore user assigned x or remove temporary x
    match savedX with
    | Some value when wasUserAssigned ->
        SymbolTable.current <- SymbolTable.add "x" value SymbolTable.current
    | _ ->
        SymbolTable.current <- SymbolTable.removeTempVariable "x" SymbolTable.current

let getInterpolationMode () : int =
    match PlotBuffer.getInterpolation() with
    | PlotBuffer.Linear -> 0
    | PlotBuffer.Spline -> 1

// Get function help text
let getFunctionHelp () : string =
    """
Available Built-in Functions:

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

Advanced Math (INT4):
  derivative(expr, x0)     - compute f'(x0)
  integrate(expr, a, b)    - compute ∫[a,b] f(x) dx
  findroot(expr, a, b)     - find root in [a,b]

Examples:
  sin(0)                → 0
  sqrt(16)              → 4
  abs(-5)               → 5
  exp(1)                → 2.71828...
  derivative(x^2, 2)    → 4
  integrate(x^2, 0, 1)  → 0.333...
  findroot(x^2-4, 0, 5) → 2
"""

// Get syntax help text
let getSyntaxHelp () : string =
    """
Syntax Guide:


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