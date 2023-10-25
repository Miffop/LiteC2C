open LiteC2C
open LiteC2C.AST
open LiteC2C.Parser


module Assembling = 
    let typeName = 
        TypeParser.parser
    let expression =  
        Parser.recursive ExpressionParser.system ExpressionParser.element
    let command = 
        let elem = 
            Parser.element (CommnadParser.allElements typeName expression)
        Parser.recursive CommnadParser.system elem
    let defention =
        let elem =
            DefenitionParser.element (DefenitionParser.allElements typeName TypeParser.name TypeParser.functionPointer expression command)
        DefenitionParser.system elem

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        1
    else
    let code = System.IO.File.ReadAllText(argv[0]) + " \n"
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
            |>Parser.run(Assembling.defention)
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