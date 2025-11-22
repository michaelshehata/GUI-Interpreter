module public Parser

open System
open NumberSystem
open Lexer
open AST

// Exceptions
let parseError = System.Exception("Parser error")

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
            (tail, Node("<NR>", [Leaf (NumberSystem.toString value)]))
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