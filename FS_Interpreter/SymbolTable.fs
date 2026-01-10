module public SymbolTable

open System
open NumberSystem

type VarType = 
    | IntType
    | FloatType
    | RatType
    | ComplexType
    | InferredType  // For untyped variables

type SymbolEntry = {
    Value: Number
    DeclaredType: VarType
}

type SymbolTable = Map<string, SymbolEntry>

// Track which variables were explicitly assigned by the user
let mutable private userAssignedVars : Set<string> = Set.empty

let empty : SymbolTable = 
    Map.empty
    |> Map.add "pi" { Value = Float Math.PI; DeclaredType = FloatType }
    |> Map.add "E" { Value = Float Math.E; DeclaredType = FloatType }

// Mark constants as user assigned (so they don't get removed)
let init() =
    userAssignedVars <- Set.empty.Add("pi").Add("E")

init()

// Add a variable to the symbol table
let add (name: string) (value: Number) (table: SymbolTable) : SymbolTable =
    Map.add name { Value = value; DeclaredType = InferredType } table

// Add a variable and mark it as user assigned
let addUserVariable (name: string) (value: Number) (table: SymbolTable) : SymbolTable =
    userAssignedVars <- userAssignedVars.Add(name)
    Map.add name { Value = value; DeclaredType = InferredType } table

// Add a variable with explicit type
let addUserVariableTyped (name: string) (value: Number) (declaredType: VarType) (table: SymbolTable) : SymbolTable =
    userAssignedVars <- userAssignedVars.Add(name)
    Map.add name { Value = value; DeclaredType = declaredType } table

// Add a temporary variable (not user assigned, can be cleared)
let addTempVariable (name: string) (value: Number) (table: SymbolTable) : SymbolTable =
    Map.add name { Value = value; DeclaredType = InferredType } table

// Check if a variable was user assigned
let isUserAssigned (name: string) : bool =
    Set.contains name userAssignedVars

// Remove temporary variables (keep user assigned ones)
let removeTempVariables (table: SymbolTable) : SymbolTable =
    table
    |> Map.filter (fun name _ -> isUserAssigned name)

// Remove a specific variable only if it's temporary
let removeTempVariable (name: string) (table: SymbolTable) : SymbolTable =
    if isUserAssigned name then
        table  // Don't remove user-assigned variables
    else
        Map.remove name table

let tryFind (name: string) (table: SymbolTable) : Number option =
    Map.tryFind name table |> Option.map (fun entry -> entry.Value)

let contains (name: string) (table: SymbolTable) : bool =
    Map.containsKey name table

// Clear all variables (for "clear" command)
let clearAll () : unit =
    userAssignedVars <- Set.empty.Add("pi").Add("E")

let mutable current : SymbolTable = empty