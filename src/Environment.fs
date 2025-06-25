module Environment

open Token
open RuntimeError
open System.Collections.Generic

type Environment(enclosing: Environment option) =
    let values = Dictionary<string, obj option>()

    new() = Environment(None)
    member x.Define(name: string, value: obj option) = values[name] <- value

    member x.Get(name: Token) =
        if values.ContainsKey name.Lexeme then
            values[name.Lexeme]
        else
            match enclosing with
            | Some env -> env.Get(name)
            | None -> raise (RuntimeError(name, $"undefined variable: '{name.Lexeme}'."))

    member x.Assign(name: Token, value: obj option) =
        if values.ContainsKey name.Lexeme then
            values[name.Lexeme] <- value
        else
            match enclosing with
            | Some env -> env.Assign(name, value)
            | None -> raise (RuntimeError(name, $"undefined variable: '{name.Lexeme}'."))
