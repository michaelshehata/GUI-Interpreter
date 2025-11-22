module Testing

open System

type TestCase = {
    Expression: string
    Expected: float
    Description: string
}

type TestResult = {
    Expression: string
    Expected: float
    Actual: float option
    Passed: bool
    ErrorMsg: string option
    Description: string
}

// Comprehensive test cases
let testCases = [
    // Basic arithmetic
    { Expression = "3 + 4 * 5"; Expected = 23.0; Description = "BODMAS test" }
    { Expression = "(3 + 4) * 5"; Expected = 35.0; Description = "Parentheses priority" }
    { Expression = "10 - 3 - 2"; Expected = 5.0; Description = "Left associativity (subtraction)" }
    { Expression = "20 / 4 / 2"; Expected = 2.5; Description = "Left associativity (division)" }
    
    // Floats
    { Expression = "3.14 + 2.5"; Expected = 5.64; Description = "Float addition" }
    { Expression = "10.5 / 2"; Expected = 5.25; Description = "Float division" }
    
    // Modulo
    { Expression = "10 % 3"; Expected = 1.0; Description = "Integer modulo" }
    { Expression = "17.5 % 5"; Expected = 2.5; Description = "Float modulo" }
    
    // Power (right-associative!)
    { Expression = "2^3"; Expected = 8.0; Description = "Simple power" }
    { Expression = "2^3^2"; Expected = 512.0; Description = "Right associativity (power)" }
    { Expression = "2^10"; Expected = 1024.0; Description = "Larger power" }
    
    // Unary minus
    { Expression = "-5 + 3"; Expected = -2.0; Description = "Unary minus" }
    { Expression = "-(3 + 4)"; Expected = -7.0; Description = "Unary minus with parentheses" }
    { Expression = "--5"; Expected = 5.0; Description = "Double unary minus" }
    { Expression = "---3"; Expected = -3.0; Description = "Triple unary minus" }
    
    // Complex expressions
    { Expression = "2 + 3 * 4 - 5"; Expected = 9.0; Description = "Mixed operators" }
    { Expression = "(2 + 3) * (4 - 1)"; Expected = 15.0; Description = "Multiple parentheses" }
    { Expression = "10 / 2 + 3 * 4"; Expected = 17.0; Description = "Division and multiplication" }
    
    // Exponential notation
    { Expression = "1.5E3"; Expected = 1500.0; Description = "Exponential notation (E)" }
    { Expression = "2.5e-2"; Expected = 0.025; Description = "Exponential notation (e-)" }
    { Expression = "1E+2"; Expected = 100.0; Description = "Exponential notation (E+)" }

    // Built-in functions
    { Expression = "sin(0)"; Expected = 0.0; Description = "sin(0)" }
    { Expression = "cos(0)"; Expected = 1.0; Description = "cos(0)" }
    { Expression = "sqrt(16)"; Expected = 4.0; Description = "sqrt(16)" }
    { Expression = "abs(-5)"; Expected = 5.0; Description = "abs(-5)" }
    { Expression = "exp(0)"; Expected = 1.0; Description = "exp(0)" }
    { Expression = "ln(1)"; Expected = 0.0; Description = "ln(1)" }
    { Expression = "floor(3.7)"; Expected = 3.0; Description = "floor(3.7)" }
    { Expression = "ceil(3.2)"; Expected = 4.0; Description = "ceil(3.2)" }
    { Expression = "round(3.5)"; Expected = 4.0; Description = "round(3.5)" }
    
    // Function composition
    { Expression = "sin(0) + cos(0)"; Expected = 1.0; Description = "Function in expression" }
    { Expression = "sqrt(4) * 3"; Expected = 6.0; Description = "Function result in multiplication" }
    { Expression = "abs(-2 + -3)"; Expected = 5.0; Description = "Function with expression argument" }
]

let runTest (testCase: TestCase) : TestResult =
    // Clear variables before each test
    API.clearVariables()
    
    try
        let result = API.interpret testCase.Expression
        let resultFloat = float result
        let passed = abs(resultFloat - testCase.Expected) < 0.0001
        {
            Expression = testCase.Expression
            Expected = testCase.Expected
            Actual = Some resultFloat
            Passed = passed
            ErrorMsg = None
            Description = testCase.Description
        }
    with
    | ex ->
        {
            Expression = testCase.Expression
            Expected = testCase.Expected
            Actual = None
            Passed = false
            ErrorMsg = Some ex.Message
            Description = testCase.Description
        }

let runAllTests () =
    let results = testCases |> List.map runTest
    let passed = results |> List.filter (fun r -> r.Passed) |> List.length
    let failed = results |> List.filter (fun r -> not r.Passed) |> List.length
    
    printfn "TEST RESULTS"
    printfn "========================================="
    printfn "Total: %d | Passed: %d | Failed: %d\n" (passed + failed) passed failed
    
    results |> List.iter (fun r ->
        if r.Passed then
            printfn "✓ PASS: %s" r.Description
            printfn "  Expression: %s = %.4f" r.Expression r.Expected
        else
            printfn "✗ FAIL: %s" r.Description
            printfn "  Expression: %s" r.Expression
            printfn "  Expected: %.4f" r.Expected
            match r.Actual with
            | Some actual -> printfn "  Got: %.4f" actual
            | None -> printfn "  Error: %s" (r.ErrorMsg |> Option.defaultValue "Unknown error")
        printfn ""
    )
    
    printfn "========================================="
    (passed, failed)