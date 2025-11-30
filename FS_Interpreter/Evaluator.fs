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
            
            // CHANGED: Handle plot(x,y) and standard functions
            match name, afterExpr with
            | "plot", Comma :: afterComma ->
                let (afterArg2, arg2Value) = E afterComma
                match afterArg2 with
                | Rpar :: rest ->
                    let x = NumberSystem.toFloat argValue
                    let y = NumberSystem.toFloat arg2Value
                    PlotBuffer.addPoint x y
                    (rest, arg2Value)
                | _ -> raise (System.Exception("Missing closing parenthesis for plot"))
                
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
    
    // Variable assignment: x = 10;
    | Ident name :: Assign :: tail ->
        let (remaining, value) = parseNeval tail
        SymbolTable.current <- SymbolTable.add name value SymbolTable.current
        
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
    
    // Execute loop
    while (if step > 0.0 then current <= endFloat else current >= endFloat) do
        // Set loop variable in symbol table
        SymbolTable.current <- SymbolTable.add varName (Float current) SymbolTable.current
        
        // Execute body - handle all statements in the body
        if not (List.isEmpty body) then
            let (_, result) = parseStatement body
            lastResult <- result
        
        // Increment
        current <- current + step
    
    lastResult