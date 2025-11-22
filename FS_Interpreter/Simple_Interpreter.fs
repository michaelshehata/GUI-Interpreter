
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