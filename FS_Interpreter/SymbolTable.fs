module SymbolTable

type SymbolTable = Map<string, float>
let mutable symbolTable : SymbolTable = Map.empty