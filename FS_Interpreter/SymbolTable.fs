module public SymbolTable

open System

open NumberSystem

type SymbolTable = Map<string, Number>

let empty : SymbolTable = 
    Map.empty
    |> Map.add "pi" (Float Math.PI)
    |> Map.add "E" (Float Math.E)

let add (name: string) (value: Number) (table: SymbolTable) : SymbolTable =
    Map.add name value table

let tryFind (name: string) (table: SymbolTable) : Number option =
    Map.tryFind name table

let contains (name: string) (table: SymbolTable) : bool =
    Map.containsKey name table

let mutable current : SymbolTable = empty