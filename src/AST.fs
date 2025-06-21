module AST

type TokenType =
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACE
    | RIGHT_BRACE
    | RIGHT_BRACKET
    | LEFT_BRACKET
    | COMMA
    | DOT
    | MINUS
    | PLUS
    | SEMICOLON
    | SLASH
    | STAR
    | BANG
    | BANG_EQUAL
    | EQUAL
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    // Tokens literais
    | IDENTIFIER
    | STRING
    | NUMBER

    // Palavras-chave
    | AND
    | CLASS
    | ELSE
    | MY_FALSE
    | FUN
    | FOR
    | IF
    | NIL
    | OR
    | PRINT
    | RETURN
    | SUPER
    | THIS
    | MY_TRUE
    | VAR
    | WHILE

    // Token para final de declaração
    | MY_EOF


type Token =
    { Type: TokenType
      Lexeme: string
      Literal: obj option
      Line: int }

type Expr =
    | Binary of Expr * Token * Expr
    | Grouping of Expr
    | Literal of obj option
    | Unary of Token * Expr
    | Variable of Token
    | Assign of Token * Expr


type Stmt =
    | Expression of Expr
    | Print of Expr
    | Var of Token * Expr option
    | Block of Stmt list
