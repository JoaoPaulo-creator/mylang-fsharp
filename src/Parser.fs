module Parser

open AST
open RuntimeError

type Parser(tokens: Token list) =
    let mutable current = 0
    let isAtEnd() = current >= tokens.Length || tokens[current].Type = MY_EOF

    let peek() = tokens[current]
    let previous() = tokens[current - 1]
    let advance() =
        if not (isAtEnd()) then current <- current + 1
        previous()

    let check tokenType = not (isAtEnd()) && peek().Type = tokenType

    let matchTokens types =
        types |> List.exists (fun t -> if check t then ignore (advance()); true else false)

    let consume tokenType message =
        if check tokenType then advance()
        else raise (RuntimeError(peek(), message))

    let synchronize() =
        let rec loop () =
            if not (isAtEnd()) then
                if previous().Type = SEMICOLON then ()
                else
                    match peek().Type with
                    | CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN -> ()
                    | _ -> ignore (advance()); loop ()
        ignore (advance())
        loop()

    let rec primary() =
        match peek().Type with
        | MY_FALSE -> ignore (advance()); Literal(Some false)
        | MY_TRUE -> ignore (advance()); Literal(Some true)
        | NIL -> ignore (advance()); Literal(None)
        | IDENTIFIER -> ignore (advance()); Variable(previous())
        | NUMBER | STRING -> 
            let token = advance()
            Literal(token.Literal)
        | LEFT_PAREN ->
            ignore (advance())
            let expr = expression()
            ignore (consume RIGHT_PAREN "Expected ')' after expression.")
            Grouping expr
        | _ -> raise (RuntimeError(peek(), "Expected expression."))

    and unary() =
        if matchTokens [BANG; MINUS] then
            let oper = previous()
            let right = unary()
            Unary(oper, right)
        else primary()

    and factor() =
        let expr = unary()
        let rec loop expr =
            if matchTokens [SLASH; STAR] then
                let oper = previous()
                let right = unary()
                loop (Binary(expr, oper, right))
            else expr
        expr |> loop

    and term() =
        let expr = factor()
        let rec loop expr =
            if matchTokens [MINUS; PLUS] then
                let oper = previous()
                let right = factor()
                loop (Binary(expr, oper, right))
            else expr
        expr |> loop

    and comparison() =
        let expr = term()
        let rec loop expr =
            if matchTokens [GREATER; GREATER_EQUAL; LESS; LESS_EQUAL] then
                let oper = previous()
                let right = term()
                loop (Binary(expr, oper, right))
            else expr
        expr |> loop

    and equality() =
        let expr = comparison()
        let rec loop expr =
            if matchTokens [BANG_EQUAL; EQUAL_EQUAL] then
                let oper = previous()
                let right = comparison()
                loop (Binary(expr, oper, right))
            else expr
        expr |> loop

    and assignment() =
        let expr = equality()
        if matchTokens [EQUAL] then
            let equals = previous()
            let value = assignment()
            match expr with
            | Variable name -> Assign(name, value)
            | _ -> raise (RuntimeError(equals, "Invalid assignment target."))
        else expr

    and expression() = assignment()

    let expressionStmt() =
        let expr = expression()
        ignore (consume SEMICOLON "Expected ';' after expression.")
        Expression expr

    let printStmt() =
        let expr = expression()
        ignore (consume SEMICOLON "Expected ';' after value.")
        Print expr

    let varDeclaration() =
        let name = consume IDENTIFIER "Expected variable name."
        let init = if matchTokens [EQUAL] then Some (expression()) else None
        ignore(consume SEMICOLON "Expected ';' after variable declaration.")
        Var(name, init)

    let rec block() =
        let mutable stmts = []
        while not (check RIGHT_BRACE) && not (isAtEnd()) do
            stmts <- declaration() :: stmts
        ignore(consume RIGHT_BRACE "Expected '}' after block.")
        List.rev stmts

    and statement() =
        if matchTokens [PRINT] then printStmt()
        elif matchTokens [LEFT_BRACE] then Block(block())
        else expressionStmt()

    and declaration() =
        try
            if matchTokens [VAR] then varDeclaration()
            else statement()
        with
        | _ -> 
            synchronize()
            Expression(Literal(None))

    member x.Parse() =
        let mutable stmts = []
        try
            while not (isAtEnd()) do
                stmts <- declaration() :: stmts
            List.rev stmts
        with
        | _ -> stmts