module public Parser

open System
open NumberSystem
open Lexer
open AST

// Exceptions
let parseError = System.Exception("Parser error")

// Parser function - validates syntax without evaluation
// Purpose: Check token list is valid 
// Arguments: tList tokens
// Returns: [] if ok, else throws
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
            let afterArg1 = E tail // Parse first argument
            match afterArg1 with 
            | Rpar :: tail -> tail // 1-argument function (e.g., sin(x))
            | Comma :: afterComma -> // Two arguments (e.g., plot(x, y))
                match E afterComma with // Parse second argument
                | Rpar :: tail -> tail
                | _ -> raise parseError
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

// Parse tree builder
// Purpose: Build parse tree for an expression
// Arguments: tList tokens
// Returns: (remaining, tree)
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
            (tail, Node("<NR>", [Leaf (NumberSystem.toString value)]))
        | Ident name :: tail ->
            (tail, Node("<NR>", [Leaf name]))
        | Func name :: Lpar :: tail ->
            let (afterArg1, arg1Tree) = E tail // Parse Arg1
            match afterArg1 with
            | Rpar :: rest ->
                (rest, Node("<NR>", [Leaf name; Leaf "("; arg1Tree; Leaf ")"])) // 1-arg func
            | Comma :: afterComma -> // Check for second argument (plot)
                let (afterArg2, arg2Tree) = E afterComma // Parse Arg2
                match afterArg2 with
                | Rpar :: rest ->
                    (rest, Node("<NR>", [Leaf name; Leaf "("; arg1Tree; Leaf ","; arg2Tree; Leaf ")"])) // 2-arg func
                | _ -> raise parseError
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
    (remaining, tree)


// Purpose: Build parse tree for full statements (assignments/loops/exprs)
// Arguments: tList tokens
// Returns: (remaining, tree)
let parseStatementTree tList = 
    
    
    let rec parseStatements tokens =
        let rec collect acc toks  = 
            match toks with
            | [] ->
                ([], List.rev acc)

            | Semicolon :: rest ->
                collect acc rest

            | _ ->
                let (remaining, stmtTree) = parseSingleStatement toks
                match remaining with
                | Semicolon :: rest ->
                    collect (stmtTree :: acc) rest
                | [] ->
                    ([], List.rev (stmtTree :: acc))
                | _ ->
                    (remaining, List.rev (stmtTree :: acc))
        collect [] tokens

    and parseSingleStatement tokens =
        match tokens with
        | Ident name :: Assign :: tail -> 
            let (remaining,expTree)= parseExpTree tail
            let stmtNode =
                Node("<statement>", [Leaf name; Leaf "=";expTree])
            (remaining, stmtNode)

        
        | For :: Ident varName :: Assign :: tail ->
            parseForLoop varName tail

        
        | _ ->
            let (remaining,expTree) = parseExpTree tokens
            let stmtNode = Node("<statement>" , [expTree])
            (remaining, stmtNode)

    
    and parseForLoop varName tail =
        
        let (afterStart,startTree) = parseExpTree tail
        match afterStart with
        | To :: afterTo ->
            
            let (afterEnd, endTree) = parseExpTree afterTo

            
            let (stepTreeOpt, afterStepPart) =
                match afterEnd with
                | Step :: afterStep ->
                    let (afterStepExpr , stepTree) = parseExpTree afterStep
                    (Some stepTree, afterStepExpr)
                | _ ->
                    (None, afterEnd)

            
            match afterStepPart with
            | Do :: bodyTokens ->
                let (remaining, bodyTree) =parseLoopBody bodyTokens

                let baseChildren =
                    [
                        Leaf "for"
                        Leaf varName
                        Leaf "="
                        startTree
                        Leaf "to"
                        endTree
                    ]

                let baseChildren =
                    match stepTreeOpt with
                    | Some s -> baseChildren @ [Leaf "step"; s]
                    | None -> baseChildren

                let forNode =
                    Node("<for-loop>", baseChildren @ [Leaf "do"; bodyTree; Leaf "end"])

                (remaining, forNode)

            | _ ->
                raise parseError

        | _ ->
            raise parseError

    
    and parseLoopBody tokens =
        let rec collect acc toks =
            match toks with
            | End :: rest ->
                
                (rest,Node("<body>", List.rev acc))

            | [] ->
                
                raise parseError

            | _ ->
                let (remaining, stmtTree) = parseSingleStatement toks
                match remaining with
                | Semicolon :: rest ->
                    collect (stmtTree :: acc) rest
                | _ ->
                    collect (stmtTree :: acc) remaining

        collect [] tokens

    
    let (remaining, stmts) = parseStatements tList

    let tree =
        match stmts with                                   
        | [single] -> single
        | _  -> Node("<statements>", stmts)

    (remaining,tree)