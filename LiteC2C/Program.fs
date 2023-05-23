open LiteC2C.AST
open LiteC2C.Parser

[<EntryPoint>]
let main argv =
    let code = System.IO.File.ReadAllText("./../../../zProg/test.txt") + " \n"
    
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
    
    let exp = 
        tokens
        |>Parser.run(ExpressionParser.Expression)
        |>Option.map(fst>>fst)
        |>Option.defaultValue (Expression.Error "parser failure")
    
    printfn "%A" exp
    
    let result = 
        exp
        |>Translation.Translate Translation.Expression.Expression
    
    printfn "%A" result

    System.Console.ReadKey()|>ignore
    0