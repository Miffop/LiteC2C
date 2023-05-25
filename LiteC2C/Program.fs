open LiteC2C
open LiteC2C.AST
open LiteC2C.Parser

[<EntryPoint>]
let main argv =
    let code = System.IO.File.ReadAllText("./../../../zProg/test.txt") + " \n"
    
    match Seq.tryFindIndex((=)'\t')code with
    |Some(x)->failwithf "за табы бан:\n%s"<|code.Substring(x)
    |None->()
    OptionMonad(){
        let! chars = 
            code
            |>List.ofSeq
            |>Parser.run(Indentation.text ' ' '\n')
            |>Option.map(fst)
        let! tokens = 
            chars
            |>Parser.run(Tokenisation.Tokenizer)
            |>Option.map(fst)
    
        tokens
        |>List.iter(printfn "%A")
    
        let! exp = 
            tokens
            |>Parser.run(DefenitionParser.file)
            |>Option.map(fst>>fst)
        printfn "%A" exp
        
        let result = 
            exp
            |>Translation.final DefenitionTranslation.defention

        match result with
        |Text(x)-> printfn "результат:\n%s" (CommandTranslation.format x)
        |Error(x)->printfn "ошибка:%s" x
        |_->printfn "неизвестная ошибка"
        |>ignore

        return ()
    }|>ignore

    System.Console.ReadKey()|>ignore
    0