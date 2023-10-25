namespace LiteC2C.Parser

open LiteC2C.AST
open Indentation.Ops

module DefenitionParser = 
    let private keyword     = Indentation.token<<Token.Op
    let private declaration = CommnadParser.declaration
    let private sameOrIndented x = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") x
    let private indented x  = Indentation.indentedScope (Token.Open "(") (Token.Close "(") x

    let globalVar typeName expression = 
        Defenition.GlobalVar <^> declaration typeName expression
    
    let functionSignature name = 
        Indentation.Monad(){
            let! fn = name
            let! parameters = Indentation.some name <|> lazy([] <^ keyword "()")
            return {Name = fn;Parameters = parameters}
        }
    
    let functionDeclaration name = 
        Defenition.Function <^> functionSignature name
    
    let functionDefention name command = 
        Indentation.Monad(){
            let! signature = functionSignature name
            let! _ = keyword "="
            let! defention = command
            return Defenition.Functiondef(signature,defention)
        }

    let structDefenition name = 
        Indentation.Monad(){
            let! _ = keyword "struct"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.Structdef(n,f)
        }
    let unionDefenition name = 
        Indentation.Monad(){
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.Uniondef(n,f)
        }
    let typedef typeName = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! t = sameOrIndented typeName
            return Defenition.Typedef(n,t)
        }
    let typedefStruct name = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "struct"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.File[Defenition.Structdef(n,f);Defenition.Typedef(n,Type.Struct n)]
        }
    let typedefUnion name = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.File[Defenition.Uniondef(n,f);Defenition.Typedef(n,Type.Union n)]
        }
    let typedefFunctionPointer functionPointer = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented functionPointer
            return Defenition.TypedefFunctionPointer(n,f)
        }
    

    let allElements typeName name functionPointer expression command = 
        [
            lazy(functionDefention name command)
            lazy(functionDeclaration name)
            lazy(structDefenition name)
            lazy(unionDefenition name)
            lazy(typedef typeName)
            lazy(typedefStruct name)
            lazy(typedefUnion name)
            lazy(typedefFunctionPointer functionPointer)
            lazy(globalVar typeName expression)
        ]

    let element elem = 
        elem
        |>Parser.choose
        |>indented

    let system defenition = 
        Defenition.File<^>Indentation.any defenition