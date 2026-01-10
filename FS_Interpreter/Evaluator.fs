module public Evaluator

open System
open NumberSystem
open Lexer
open SymbolTable
open Parser
open PlotBuffer

// Exceptions
let runtimeError = System.Exception("Runtime error")

// Parser and evaluator function - parses and computes result
let rec parseNeval tList = 
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
            
            // CHANGED: Handle plot(x,y), derivative, integrate, findroot, and standard functions
            match name, afterExpr with
            // Two-argument plot function
            | "plot", Comma :: afterComma ->
                let (afterArg2, arg2Value) = E afterComma
                match afterArg2 with
                | Rpar :: rest ->
                    let x = NumberSystem.toFloat argValue
                    let y = NumberSystem.toFloat arg2Value
                    PlotBuffer.addPoint x y
                    (rest, arg2Value)
                | _ -> raise (System.Exception("Missing closing parenthesis for plot"))
            
            // INT4: derivative(expr, x0)
            | "derivative", Comma :: afterComma ->
                let (afterArg2, arg2Value) = E afterComma
                match afterArg2 with
                | Rpar :: rest ->
                    // Extract expression tokens before first comma
                    let rec extractExprTokens acc tokens =
                        match tokens with
                        | Comma :: _ -> List.rev acc
                        | t :: ts -> extractExprTokens (t :: acc) ts
                        | [] -> List.rev acc
                    
                    let exprTokens = extractExprTokens [] tail
                    let x0 = NumberSystem.toFloat arg2Value
                    let stepSize = 1e-4
                        
                        
                    
                    // Save current x if it exists and is user-assigned
                    let savedX = SymbolTable.tryFind "x" SymbolTable.current
                    let wasUserX = SymbolTable.isUserAssigned "x"
                    
                    // Evaluate at x0 - stepSize
                    SymbolTable.current <- SymbolTable.addTempVariable "x" (Float (x0 - stepSize)) SymbolTable.current
                    let (_, leftVal) = parseNeval exprTokens
                    let leftFloat = NumberSystem.toFloat leftVal
                    
                    // Evaluate at x0 + stepSize
                    SymbolTable.current <- SymbolTable.addTempVariable "x" (Float (x0 + stepSize)) SymbolTable.current
                    let (_, rightVal) = parseNeval exprTokens
                    let rightFloat = NumberSystem.toFloat rightVal
                    
                    // Restore x if it was user-assigned, otherwise remove
                    match savedX, wasUserX with
                    | Some value, true ->
                        SymbolTable.current <- SymbolTable.add "x" value SymbolTable.current
                    | _ ->
                        SymbolTable.current <- SymbolTable.removeTempVariable "x" SymbolTable.current
                    
                    let deriv = (rightFloat - leftFloat) / (2.0 * stepSize)
                    (rest, Float deriv)
                | _ -> raise (System.Exception("derivative requires 2 arguments: (expression, x_value)"))
            
            // INT4: integrate(expr, a, b)
            | "integrate", Comma :: afterComma ->
                let (afterArg2, arg2Value) = E afterComma
                match afterArg2 with
                | Comma :: afterComma2 ->
                    let (afterArg3, arg3Value) = E afterComma2
                    match afterArg3 with
                    | Rpar :: rest ->
                        let rec extractExprTokens acc tokens =
                            match tokens with
                            | Comma :: _ -> List.rev acc
                            | t :: ts -> extractExprTokens (t :: acc) ts
                            | [] -> List.rev acc
                        
                        let exprTokens = extractExprTokens [] tail
                        let a = NumberSystem.toFloat arg2Value
                        let b = NumberSystem.toFloat arg3Value
                        let steps = 1000
                        
                        // Save x state
                        let savedX = SymbolTable.tryFind "x" SymbolTable.current
                        let wasUserX = SymbolTable.isUserAssigned "x"
                        
                        // Trapezoidal rule
                        let mutable startX = a
                        let mutable endX = b
                        let swapped = if startX > endX then
                                        let tmp = startX
                                        startX <- endX
                                        endX <- tmp
                                        true
                                      else false
                        
                        let h = (endX - startX) / float steps
                        
                        // Evaluate at start
                        SymbolTable.current <- SymbolTable.addTempVariable "x" (Float startX) SymbolTable.current
                        let (_, startVal) = parseNeval exprTokens
                        let fStart = NumberSystem.toFloat startVal
                        
                        // Evaluate at end
                        SymbolTable.current <- SymbolTable.addTempVariable "x" (Float endX) SymbolTable.current
                        let (_, endVal) = parseNeval exprTokens
                        let fEnd = NumberSystem.toFloat endVal
                        
                        // Sum interior points
                        let mutable sum = 0.0
                        for i = 1 to steps - 1 do
                            let x = startX + float i * h
                            SymbolTable.current <- SymbolTable.addTempVariable "x" (Float x) SymbolTable.current
                            let (_, midVal) = parseNeval exprTokens
                            sum <- sum + NumberSystem.toFloat midVal
                        
                        // Restore x
                        match savedX, wasUserX with
                        | Some value, true ->
                            SymbolTable.current <- SymbolTable.add "x" value SymbolTable.current
                        | _ ->
                            SymbolTable.current <- SymbolTable.removeTempVariable "x" SymbolTable.current
                        
                        let area = h * ((fStart + fEnd) / 2.0 + sum)
                        let finalArea = if swapped then -area else area
                        
                        (rest, Float finalArea)
                    | _ -> raise (System.Exception("Missing closing parenthesis for integrate"))
                | _ -> raise (System.Exception("integrate requires 3 arguments: (expression, a, b)"))
            
            // INT4: findroot(expr, a, b)
            | "findroot", Comma :: afterComma ->
                let (afterArg2, arg2Value) = E afterComma
                match afterArg2 with
                | Comma :: afterComma2 ->
                    let (afterArg3, arg3Value) = E afterComma2
                    match afterArg3 with
                    | Rpar :: rest ->
                        let rec extractExprTokens acc tokens =
                            match tokens with
                            | Comma :: _ -> List.rev acc
                            | t :: ts -> extractExprTokens (t :: acc) ts
                            | [] -> List.rev acc
                        
                        let exprTokens = extractExprTokens [] tail
                        let a = NumberSystem.toFloat arg2Value
                        let b = NumberSystem.toFloat arg3Value
                        
                        // Save x state
                        let savedX = SymbolTable.tryFind "x" SymbolTable.current
                        let wasUserX = SymbolTable.isUserAssigned "x"
                        
                        // Bisection method
                        let tolerance = 1e-6
                        let maxIterations = 100
                        
                        let evalAt x =
                            SymbolTable.current <- SymbolTable.addTempVariable "x" (Float x) SymbolTable.current
                            let (_, result) = parseNeval exprTokens
                            NumberSystem.toFloat result
                        
                        let mutable left = a
                        let mutable right = b
                        let mutable fLeft = evalAt left
                        let mutable fRight = evalAt right
                        
                        if fLeft * fRight > 0.0 then
                            raise (System.Exception("Function must have opposite signs at a and b"))
                        
                        let mutable root = (left + right) / 2.0
                        let mutable i = 0
                        
                        while i < maxIterations && (right - left) / 2.0 > tolerance do
                            root <- (left + right) / 2.0
                            let fMid = evalAt root
                            
                            if abs fMid < tolerance then
                                left <- root
                                right <- root
                            elif fLeft * fMid < 0.0 then
                                right <- root
                                fRight <- fMid
                            else
                                left <- root
                                fLeft <- fMid
                            
                            i <- i + 1
                        
                        // Restore x
                        match savedX, wasUserX with
                        | Some value, true ->
                            SymbolTable.current <- SymbolTable.add "x" value SymbolTable.current
                        | _ ->
                            SymbolTable.current <- SymbolTable.removeTempVariable "x" SymbolTable.current
                        
                        (rest, Float root)
                    | _ -> raise (System.Exception("Missing closing parenthesis for findroot"))
                | _ -> raise (System.Exception("findroot requires 3 arguments: (expression, a, b)"))
                
            // Standard single-argument functions
            | _, Rpar :: rest ->
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
                    | "log" -> Float (Math.Log10(argFloat))
                    | "ln" -> Float (Math.Log(argFloat))
                    // Utility functions
                    | "sqrt" -> 
                        if argFloat < 0.0 then 
                            raise (System.Exception("Cannot take square root of negative number"))
                        Float (Math.Sqrt(argFloat))
                    | "abs" -> Float (Math.Abs(argFloat))
                    | "floor" -> Float (Math.Floor(argFloat))
                    | "ceil" -> Float (Math.Ceiling(argFloat))
                    | "round" -> Float (Math.Round(argFloat))
                    // INT 3: Plotting controls
                    | "interpolation" ->
                        if argFloat >= 1.0 then 
                            PlotBuffer.setInterpolation PlotBuffer.Spline
                            Float 1.0
                        else 
                            PlotBuffer.setInterpolation PlotBuffer.Linear
                            Float 0.0
                    | "i" -> Complex(0.0, argFloat)
                    | _ -> raise (System.Exception($"Unknown function: {name}"))
                (rest, result)
            | _ -> raise (System.Exception($"Missing closing parenthesis or invalid arguments for function '{name}'"))

        | Lpar :: tail -> 
            let (tLst, tval) = E tail
            match tLst with 
            | Rpar :: tail -> (tail, tval)
            | _ -> raise (System.Exception("Missing closing parenthesis"))
        | [] -> 
            raise (System.Exception("Unexpected end of expression"))
        | Assign :: _ ->
            raise (System.Exception("Unexpected '='. Assignment requires: variable = expression"))
        | token :: _ -> 
            raise (System.Exception($"Unexpected token: {token}. Expected number, variable, or function"))
    
    let (remaining, result) = E tList
    (remaining, result)

// Statement parser - handles assignments, for loops, and expressions
// Must use 'and' to make it mutually recursive with executeForLoop
and parseStatement tList = 
    match tList with
    | [] -> ([], Integer 0L)
    
    // ADDED: Static typing - int x = 10;
    | Lexer.IntType :: Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        let intValue = NumberSystem.coerceToInt value
        SymbolTable.current <- SymbolTable.addUserVariableTyped name intValue SymbolTable.VarType.IntType SymbolTable.current
        
        match remaining with
        | Semicolon :: rest -> 
            if List.isEmpty rest then ([], intValue)
            else parseStatement rest
        | [] -> ([], intValue)
        | _ -> raise Parser.parseError
    
    // ADDED: Static typing - float x = 10;
    | Lexer.FloatType :: Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        let floatValue = NumberSystem.coerceToFloat value
        SymbolTable.current <- SymbolTable.addUserVariableTyped name floatValue SymbolTable.VarType.FloatType SymbolTable.current
        
        match remaining with
        | Semicolon :: rest -> 
            if List.isEmpty rest then ([], floatValue)
            else parseStatement rest
        | [] -> ([], floatValue)
        | _ -> raise Parser.parseError
    
    // ADDED: Static typing - rat x = 3;
    | Lexer.RatType :: Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        let ratValue = NumberSystem.coerceToRational value
        SymbolTable.current <- SymbolTable.addUserVariableTyped name ratValue SymbolTable.VarType.RatType SymbolTable.current
        
        match remaining with
        | Semicolon :: rest -> 
            if List.isEmpty rest then ([], ratValue)
            else parseStatement rest
        | [] -> ([], ratValue)
        | _ -> raise Parser.parseError
    
    // ADDED: Static typing - complex x = 3;
    | Lexer.ComplexType :: Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        let complexValue = NumberSystem.coerceToComplex value
        SymbolTable.current <- SymbolTable.addUserVariableTyped name complexValue SymbolTable.VarType.ComplexType SymbolTable.current
        
        match remaining with
        | Semicolon :: rest -> 
            if List.isEmpty rest then ([], complexValue)
            else parseStatement rest
        | [] -> ([], complexValue)
        | _ -> raise Parser.parseError
    
    // Variable assignment: x = 10;
    | Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        // CHANGED: Mark as user-assigned variable
        SymbolTable.current <- SymbolTable.addUserVariable name value SymbolTable.current
        
        match remaining with
        | Semicolon :: rest -> 
            if List.isEmpty rest then
                ([], value)  // End of input
            else
                parseStatement rest  // Continue processing
        | [] -> ([], value)
        | _ -> raise Parser.parseError
    
    // For loop: for x = 1 to 5 do ... end
    | For :: Ident varName :: Assign :: tail ->
        // Parse start value
        let (afterStart, startVal) = parseNeval tail
        
        match afterStart with
        | To :: afterTo ->
            // Parse end value
            let (afterEnd, endVal) = parseNeval afterTo
            
            // Check for optional step and extract body start
            let (stepVal, bodyTokens) = 
                match afterEnd with
                | Step :: afterStepKeyword ->
                    let (afterStepVal, stepNum) = parseNeval afterStepKeyword
                    match afterStepVal with
                    | Do :: rest -> (Some stepNum, rest)
                    | _ -> raise (System.Exception("Expected 'do' after step value"))
                | Do :: rest -> 
                    (None, rest)
                | _ -> 
                    raise (System.Exception("Expected 'step' or 'do' in for loop"))
            
            // Find the matching 'end'
            let rec findEnd tokens acc depth =
                match tokens with
                | [] -> raise (System.Exception("Missing 'end' for 'for' loop"))
                | End :: rest when depth = 0 -> (List.rev acc, rest)
                | For :: rest -> findEnd rest (For :: acc) (depth + 1)
                | End :: rest -> findEnd rest (End :: acc) (depth - 1)
                | token :: rest -> findEnd rest (token :: acc) depth
            
            let (body, remaining) = findEnd bodyTokens [] 0
            
            // Execute the for loop
            let loopResult = executeForLoop varName startVal endVal stepVal body
            
            // Check what comes after the loop
            if List.isEmpty remaining then
                ([], loopResult)
            else
                // Continue processing remaining tokens
                parseStatement remaining
        | _ -> raise (System.Exception("Expected 'to' in for loop"))
    
    // Regular expression
    | _ -> 
        let (remaining, result) = parseNeval tList
        if List.isEmpty remaining then
            ([], result)
        else if remaining.Head = Semicolon && remaining.Tail <> [] then
            // Skip semicolon and continue
            parseStatement remaining.Tail
        else if remaining.Head = Semicolon && remaining.Tail = [] then
            ([], result)
        else
            (remaining, result)

// Loop execution - uses 'and' for mutual recursion with parseStatement
and executeForLoop (varName: string) (startVal: Number) (endVal: Number) (stepVal: Number option) (body: Terminal list) =
    let step = match stepVal with
               | Some s -> NumberSystem.toFloat s
               | None -> 1.0
    
    let startFloat = NumberSystem.toFloat startVal
    let endFloat = NumberSystem.toFloat endVal
    
    let mutable current = startFloat
    let mutable lastResult = Integer 0L
    

    let wasUserAssigned = SymbolTable.isUserAssigned varName
    
    // Execute loop
    while (if step > 0.0 then current <= endFloat else current >= endFloat) do
        // Set loop variable in symbol table - mark as user assigned
        SymbolTable.current <- SymbolTable.addUserVariable varName (Float current) SymbolTable.current
        
        // Execute body - handle all statements in the body
        if not (List.isEmpty body) then
            let (_, result) = parseStatement body
            lastResult <- result
        
        // Increment
        current <- current + step
    
    
    lastResult