module public Tests

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
    
    // Variables (INT2)
    { Expression = "x = 5; x + 3"; Expected = 8.0; Description = "Variable assignment and usage" }
    { Expression = "y = 10; y * 2"; Expected = 20.0; Description = "Variable in multiplication" }
    { Expression = "a = 3; b = 4; a^2 + b^2"; Expected = 25.0; Description = "Multiple variables" }
]

// For Loop Test Cases 
let forLoopTestCases = [
    // Basic for loops
    { Expression = "sum = 0; for i = 1 to 5 do sum = sum + i end sum"; Expected = 15.0; Description = "Sum 1 to 5" }
    { Expression = "sum = 0; for i = 1 to 10 do sum = sum + i end sum"; Expected = 55.0; Description = "Sum 1 to 10" }
    { Expression = "product = 1; for i = 1 to 5 do product = product * i end product"; Expected = 120.0; Description = "Factorial of 5" }
    
    // For loops with step
    { Expression = "sum = 0; for i = 0 to 10 step 2 do sum = sum + i end sum"; Expected = 30.0; Description = "Sum even numbers 0-10" }
    { Expression = "sum = 0; for i = 1 to 10 step 3 do sum = sum + i end sum"; Expected = 22.0; Description = "Sum with step 3" }
    { Expression = "sum = 0; for i = 10 to 1 step -1 do sum = sum + i end sum"; Expected = 55.0; Description = "Countdown loop" }
    { Expression = "sum = 0; for i = 10 to 0 step -2 do sum = sum + i end sum"; Expected = 30.0; Description = "Even countdown" }
    
    // For loop with expressions
    { Expression = "for i = 1 to 3 do x = i * 2 end x"; Expected = 6.0; Description = "Loop variable in expression" }
    { Expression = "sum = 0; for i = 1 to 5 do sum = sum + i^2 end sum"; Expected = 55.0; Description = "Sum of squares" }
    { Expression = "sum = 0; for i = 1 to 4 do sum = sum + 2^i end sum"; Expected = 30.0; Description = "Sum of powers of 2" }
    
    // For loop accessing the variable
    { Expression = "for x = 1 to 5 do y = x end y"; Expected = 5.0; Description = "Final loop variable value" }
    { Expression = "for x = 0 to 3 do z = x * x end z"; Expected = 9.0; Description = "Last iteration result" }
    
    // For loop with functions
    { Expression = "sum = 0; for i = 0 to 3 do sum = sum + sqrt(i) end sum"; Expected = 4.146; Description = "Sum with sqrt function" }
    { Expression = "sum = 0; for i = 1 to 3 do sum = sum + abs(-i) end sum"; Expected = 6.0; Description = "Sum with abs function" }
]

let runTest (testCase: TestCase) : TestResult =
    // Clear variables before each test
    API.clearVariables()
    
    try
        let result = API.interpret testCase.Expression
        let resultFloat = float result
        let tolerance = 0.001 // Increased tolerance for floating point
        let passed = abs(resultFloat - testCase.Expected) < tolerance
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
    printfn "BASIC TESTS (INT1 & INT2)"
    printfn "========================================="
    
    let basicResults = testCases |> List.map runTest
    let basicPassed = basicResults |> List.filter (fun r -> r.Passed) |> List.length
    let basicFailed = basicResults |> List.filter (fun r -> not r.Passed) |> List.length
    
    basicResults |> List.iter (fun r ->
        if r.Passed then
            printfn "✓ PASS: %s" r.Description
        else
            printfn "✗ FAIL: %s" r.Description
            printfn "  Expression: %s" r.Expression
            printfn "  Expected: %.4f" r.Expected
            match r.Actual with
            | Some actual -> printfn "  Got: %.4f" actual
            | None -> printfn "  Error: %s" (r.ErrorMsg |> Option.defaultValue "Unknown error")
    )
    
    printfn "\n========================================="
    printfn "Basic Tests: %d passed, %d failed\n" basicPassed basicFailed
    
    // For loop tests
    printfn "FOR LOOP TESTS (INT3)"
    printfn "========================================="
    
    let loopResults = forLoopTestCases |> List.map runTest
    let loopPassed = loopResults |> List.filter (fun r -> r.Passed) |> List.length
    let loopFailed = loopResults |> List.filter (fun r -> not r.Passed) |> List.length
    
    loopResults |> List.iter (fun r ->
        if r.Passed then
            printfn "✓ PASS: %s" r.Description
        else
            printfn "✗ FAIL: %s" r.Description
            printfn "  Expression: %s" r.Expression
            printfn "  Expected: %.4f" r.Expected
            match r.Actual with
            | Some actual -> printfn "  Got: %.4f" actual
            | None -> printfn "  Error: %s" (r.ErrorMsg |> Option.defaultValue "Unknown error")
    )
    
    printfn "\n========================================="
    printfn "For Loop Tests: %d passed, %d failed\n" loopPassed loopFailed
    
    let totalPassed = basicPassed + loopPassed
    let totalFailed = basicFailed + loopFailed
    
    printfn "========================================="
    printfn "TOTAL: %d passed, %d failed" totalPassed totalFailed
    printfn "========================================="
    
    (totalPassed, totalFailed)