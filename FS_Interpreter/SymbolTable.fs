module SymbolTable

open NumberSystem

type SymbolTable = Map<string, Number>

let empty : SymbolTable = Map.empty

let add (name: string) (value: Number) (table: SymbolTable) : SymbolTable =
    Map.add name value table

let tryFind (name: string) (table: SymbolTable) : Number option =
    Map.tryFind name table

let contains (name: string) (table: SymbolTable) : bool =
    Map.containsKey name table

let mutable current : SymbolTable = empty