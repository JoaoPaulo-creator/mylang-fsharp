module Scanner

open System
open AST
open RuntimeError

type Scanner(source: string) =
    let keywords =
        dict [ "true", MY_TRUE; "false", MY_FALSE; "nil", NIL; "var", VAR; "print", PRINT ]

    let mutable tokens = []
    let mutable start = 0
    let mutable current = 0
    let mutable line = 1

    let isAtEnd () = current >= source.Length
    let advance () =
        let c = source.[current]
        current <- current + 1
        c

    let peek() = if isAtEnd() then '\000' else source.[current]

    let peekNext() =
        if current + 1 >= source.Length then '\000' else source.[current + 1]

    let matchChar expected =
        if isAtEnd() || source.[current] <> expected then
            false
        else
            current <- current + 1
            true

    let isDigit c = c >= '0' && c <= '9'
    let isAlpha c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c = '_'
    let isAlphaNumeric c = isAlpha c || isDigit c

    let addToken tokenType literal =
        let text = source.Substring(start, current - start)
        tokens <- { Type = tokenType; Lexeme = text; Literal = literal; Line = line } :: tokens

    let string() =
        while peek() <> '"' && not (isAtEnd()) do
            if peek() = '\n' then line <- line + 1
            ignore (advance())
        if isAtEnd() then
            raise (RuntimeError({ Type = MY_EOF; Lexeme = ""; Literal = None; Line = line }, "Unterminated string."))
        ignore (advance())
        let value = source.Substring(start + 1, current - start - 2)
        addToken STRING (Some (box value))

    let number() =
        while isDigit(peek()) do ignore (advance())
        if peek() = '.' && isDigit(peekNext()) then
            ignore (advance())
            while isDigit(peek()) do ignore (advance())
        let text = source.Substring(start, current - start)
        try
            let value = double text
            addToken NUMBER (Some (box value))
        with
        | :? FormatException ->
            raise (RuntimeError({ Type = NUMBER; Lexeme = text; Literal = None; Line = line }, $"Invalid number format: '{text}'."))

    let identifier() =
        while isAlphaNumeric (peek()) do ignore (advance())
        let text = source.Substring(start, current - start)
        let tokenType = if keywords.ContainsKey text then keywords.[text] else IDENTIFIER
        addToken tokenType None

    let scanToken() =
        let c = advance()
        match c with
        | '(' -> addToken LEFT_PAREN None
        | ')' -> addToken RIGHT_PAREN None
        | '{' -> addToken LEFT_BRACE None
        | '}' -> addToken RIGHT_BRACE None
        | '[' -> addToken LEFT_BRACKET None
        | ']' -> addToken RIGHT_BRACKET None
        | ',' -> addToken COMMA None
        | '.' -> addToken DOT None
        | '-' -> addToken MINUS None
        | '+' -> addToken PLUS None
        | ';' -> addToken SEMICOLON None
        | '*' -> addToken STAR None
        | '!' -> addToken (if matchChar '=' then BANG_EQUAL else BANG) None
        | '=' -> addToken (if matchChar '=' then EQUAL_EQUAL else EQUAL) None
        | '<' -> addToken (if matchChar '=' then LESS_EQUAL else LESS) None
        | '>' -> addToken (if matchChar '=' then GREATER_EQUAL else GREATER) None
        | '/' ->
            if matchChar '/' then
                while peek() <> '\n' && not (isAtEnd()) do ignore (advance())
            else addToken SLASH None
        | ' ' | '\r' | '\t' -> ()
        | '\n' -> line <- line + 1
        | '"' -> string()
        | _ ->
            if isDigit c then number()
            elif isAlpha c then identifier()
            else raise (RuntimeError({ Type = MY_EOF; Lexeme = ""; Literal = None; Line = line }, "Unexpected character."))

    member x.ScanTokens() =
        while not (isAtEnd()) do
            start <- current
            scanToken()
        tokens <- { Type = MY_EOF; Lexeme = ""; Literal = None; Line = line } :: tokens
        List.rev tokens