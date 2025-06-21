open My

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> My.RunPrompt()
    | 1 -> My.RunFile(argv.[0])
    | _ ->
        eprintfn "Usage: %s [file]" argv.[0]
        exit(1)
    0