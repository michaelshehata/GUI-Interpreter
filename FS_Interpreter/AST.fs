module AST

open System.Text


type ParseTree =
    | Node of string * ParseTree list
    | Leaf of string

let toString (tree: ParseTree) : string = 
    let sb = StringBuilder()
    
    let rec print indent t =
        let ind = String.replicate indent "  "
        match t with
        | Leaf text ->
            sb.AppendLine(sprintf "%s%s" ind text) |> ignore
        | Node (label, children) ->
            sb.AppendLine(sprintf "%s%s" ind label) |> ignore
            children |> List.iter (print (indent + 1))
    
    print 0 tree
    sb.ToString()





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




