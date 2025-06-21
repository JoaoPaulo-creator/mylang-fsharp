module Interpreter

open AST
open RuntimeError
open Environment
open System

type Interpreter() =
    let global_ = Environment()
    let mutable curr_env = global_

    let isTruthy(value: obj option) =
        match value with
        | None -> false
        | Some v ->
            match v with
            | :? bool as b -> b
            | _ -> true

    let checkNumberOperand(oper: Token, operand: obj option) =
        match operand with
        | Some (:? double) -> ()
        | _ -> raise (RuntimeError(oper, "Operand must be a number."))

    let checkNumberOperands(oper: Token, left: obj option, right: obj option) =
        match (left, right) with
        | Some (:? double), Some (:? double) -> ()
        | _ -> raise (RuntimeError(oper, "Operands must be numbers."))

    let isEqual(a: obj option, b: obj option) =
        match (a, b) with
        | None, None -> true
        | None, _ | _, None -> false
        | Some va, Some vb ->
            match (va, vb) with
            | (:? double as l), (:? double as r) -> l = r
            | (:? string as l), (:? string as r) -> l = r
            | (:? bool as l), (:? bool as r) -> l = r
            | _ -> false

    let stringify(value: obj option) =
        match value with
        | None -> "nil"
        | Some v ->
            match v with
            | :? double as d ->
                let text = d.ToString()
                if text.EndsWith(".0") then text.Substring(0, text.Length - 2) else text
            | :? string as s -> s
            | :? bool as b -> b.ToString().ToLower()
            | _ -> "Unknown type"

    let rec eval(expr: Expr) =
        match expr with
        | Literal v -> v
        | Grouping expr -> eval expr
        | Unary(oper, right) ->
            let r = eval right
            match oper.Type with
            | BANG -> Some (box (not (isTruthy r)))
            | MINUS ->
                checkNumberOperand(oper, r)
                match r with
                | Some (:? double as d) -> Some (box (-d))
                | _ -> None
            | _ -> None
        | Binary(left, oper, right) ->
            let l = eval left
            let r = eval right
            match oper.Type with
            | GREATER ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l > r))
                | _ -> None
            | GREATER_EQUAL ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l >= r))
                | _ -> None
            | LESS ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l < r))
                | _ -> None
            | LESS_EQUAL ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l <= r))
                | _ -> None
            | MINUS ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l - r))
                | _ -> None
            | PLUS ->
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l + r))
                | Some (:? string as l), Some (:? string as r) -> Some (box (l + r))
                | _ -> raise (RuntimeError(oper, "Operands must be numbers or strings."))
            | STAR ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l * r))
                | _ -> None
            | SLASH ->
                checkNumberOperands(oper, l, r)
                match (l, r) with
                | Some (:? double as l), Some (:? double as r) -> Some (box (l / r))
                | _ -> None
            | BANG_EQUAL -> Some (box (not (isEqual(l, r))))
            | EQUAL_EQUAL -> Some (box (isEqual(l, r)))
            | _ -> None
        | Variable name -> curr_env.Get name
        | Assign(name, value) ->
            let v = eval value
            curr_env.Assign(name, v)
            v

    let rec execute(stmt: Stmt) =
        match stmt with
        | Expression expr -> ignore (eval expr)
        | Print expr -> printfn "%s" (stringify (eval expr))
        | Var(name, init) ->
            let value = match init with
                        | Some expr -> eval expr
                        | None -> None
            curr_env.Define(name.Lexeme, value)
        | Block stmts ->
            let previous = curr_env
            try
                curr_env <- Environment(Some curr_env)
                stmts |> List.iter execute
            finally
                curr_env <- previous

    member x.Interpret(stmts: Stmt list) =
        try
            stmts |> List.iter execute
        with
        | RuntimeError(token, msg) ->
            printfn "Runtime Error [Line %d]: %s" token.Line msg