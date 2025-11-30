// Simple Interpreter in F#
// Author: R.J. Lapeer 
// Date: 23/10/2022
// Reference: Peter Sestoft, Grammars and parsing with F#, Tech. Report

// Group 21 Advanced Programming
// Michael Shehata, Ali Jamjoum, Luke Wilson


module public Program

open System

// Function to get input from console
let getInputString () : string = 
    Console.Write("Enter an expression: ")
    Console.ReadLine()

// Entry point
[<EntryPoint>]
let main argv =
    Console.WriteLine("Simple Interpreter - Starting Tests...")
    Console.WriteLine(String.replicate 50 "=")
    
    // Run automated tests
    let (passed, failed) = Tests.runAllTests()
    
    // Provide feedback
    if failed = 0 then
        Console.ForegroundColor <- ConsoleColor.Green
        Console.WriteLine("\n✓ ALL TESTS PASSED!\n")
        Console.ResetColor()
    else
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine($"\n✗ WARNING: {failed} test(s) FAILED.\n")
        Console.ResetColor()
    
    // Interactive mode
    Console.WriteLine(String.replicate 50 "=")
    Console.WriteLine("Interactive Mode - Enter expressions below:")
    Console.WriteLine(String.replicate 50 "=")
    
    let input = getInputString()
    
    try
        let oList = Lexer.lexer input
        let _ = API.printTList oList
        let Out = Evaluator.parseStatement oList
        Console.WriteLine("Result = {0}", snd Out)
        0
    with
    | ex ->
        Console.ForegroundColor <- ConsoleColor.Red
        Console.WriteLine("Error: {0}", ex.Message)
        Console.ResetColor()
        1

// Grammar in BNF: (updated for loops)
// <Statement>     ::= <Assignment> | <ForLoop> | <E>
// <Assignment>    ::= <Ident> "=" <E> ";"?
// <ForLoop>       ::= "for" <Ident> "=" <E> "to" <E> ("step" <E>)? "do" <Body> "end"
// <Body>          ::= <Statement>*
// <E>             ::= <T> <Eopt>
// <Eopt>          ::= "+" <T> <Eopt> | "-" <T> <Eopt> | <empty>
// <T>             ::= <P> <Topt>
// <Topt>          ::= "*" <P> <Topt> | "/" <P> <Topt> | "%" <P> <Topt> | <empty>
// <P>             ::= <U> <Popt>
// <Popt>          ::= "^" <P> | <empty>
// <U>             ::= "-" <U> | <NR>
// <NR>            ::= <Num> | <Ident> | <Func> "(" <E> ")" | "(" <E> ")"