module My

open Scanner
open Parser
open Interpreter
open RuntimeError
open System
open System.IO

type My() =
    static member RunFile(path: string) =
        if not (File.Exists(path)) then
            eprintfn "Error: %s not found." path
            exit(66)
        try
            let source = File.ReadAllText(path)
            My.Run(source)
        with
        | :? IOException as ex ->
            eprintfn "IO error reading file %s: %s" path ex.Message
            exit(77)
        | ex ->
            eprintfn "unexpected error reading file %s: %s" path ex.Message
            exit(77)

    static member RunPrompt() =
        let rec loop() =
            printf "my> "
            let line = Console.ReadLine()
            if isNull line || line = "exit" then ()
            else
                    try
                        My.Run(line)
                        loop()
                    with
                    | RuntimeError(token, msg) ->
                        printfn "Error [Line %d]: %s" token.Line msg
                        loop()
        loop()

    static member Run(source: string) =
        try
            let tokens = Scanner(source).ScanTokens()
            let statements = Parser(tokens).Parse()
            let interpreter = Interpreter()
            interpreter.Interpret(statements)
        with
        | RuntimeError(token, msg) ->
            eprintfn "Runtime error [line %d]: %s" token.Line msg
            exit(1)
        | ex ->
            eprintfn "Unexpected error in Run: %s" ex.Message
            exit(1)
        ()