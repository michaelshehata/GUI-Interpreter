module AST

open System.Text

type ParseTree =
    | Node of string * ParseTree list
    | Leaf of string

// Convert parse tree to string for visualization
let parseTreeToString (tree: ParseTree) : string = 
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