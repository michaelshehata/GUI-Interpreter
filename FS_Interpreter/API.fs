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