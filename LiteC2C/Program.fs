open LiteC2C.Parser

[<EntryPoint>]
let main argv =
    let code = System.IO.File.ReadAllText("./../../../zProg/Factorial.txt") + " \n"
    
    match Seq.tryFindIndex((=)'\t')code with
    |Some(x)->failwithf "за табы бан:\n%s"<|code.Substring(x)
    |None->()

    let chars = 
        code
        |>List.ofSeq
        |>Parser.run(Indentation.text ' ' '\n')
        |>Option.map(fst)
        |>Option.defaultValue []
    let tokens = 
        chars
        |>Parser.run(Tokenisation.Tokenizer)
        |>Option.map(fst)
        |>Option.defaultValue []
        
    tokens
    |>List.iter(printfn "%A")

    System.Console.ReadKey()|>ignore
    0