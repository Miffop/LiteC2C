namespace LiteC2C.Parser

open LiteC2C.AST
open Indentation.Ops

module DefenitionParser = 
    let private expression  = ExpressionParser.Expression
    let private typeName    = TypeParser.parser
    let private name        = TypeParser.name
    let private codeblock   = CommnadParser.codeblock
    let private keyword     = Indentation.token<<Token.Op
    let private declaration = CommnadParser.declaration
    let private sameOrIndented x = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") x
    let private indented x  = Indentation.indentedScope (Token.Open "(") (Token.Close "(") x

    let globalVar = 
        Defenition.GlobalVar<^>declaration
    
    let functionSignature = 
        Indentation.Monad(){
            let! fn = name
            let! parameters = Indentation.some name <|> lazy([] <^ keyword "()")
            return {Name = fn;Parameters = parameters}
        }
    
    let functionDeclaration = 
        Defenition.Function <^> functionSignature
    
    let functionDefention = 
        Indentation.Monad(){
            let! signature = functionSignature
            let! _ = keyword "="
            let! defention = codeblock
            return Defenition.Functiondef(signature,defention)
        }

    let structDefenition = 
        Indentation.Monad(){
            let! _ = keyword "struct"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.Structdef(n,f)
        }
    let unionDefenition = 
        Indentation.Monad(){
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.Uniondef(n,f)
        }
    let typedef = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! t = sameOrIndented typeName
            return Defenition.Typedef(n,t)
        }
    let typedefStruct = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "struct"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.File[Defenition.Structdef(n,f);Defenition.Typedef(n,Type.Struct n)]
        }
    let typedefUnion = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented (Indentation.any name)
            return Defenition.File[Defenition.Uniondef(n,f);Defenition.Typedef(n,Type.Union n)]
        }
    let typedefFunctionPointer = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = sameOrIndented TypeParser.functionPointer
            return Defenition.TypedefFunctionPointer(n,f)
        }
    
    
    let defention = 
        [
            lazy(structDefenition)
            lazy(unionDefenition)
            lazy(typedefStruct)
            lazy(typedefUnion)
            lazy(typedefFunctionPointer)
            lazy(typedef)
            lazy(functionDefention)
            lazy(functionDeclaration)
            lazy(globalVar)
        ]
        |>Parser.choose
        |>indented
    let file = 
        Defenition.File<^>Indentation.any defention