namespace LiteC2C.Parser

open LiteC2C.AST
open Indentation.Ops

module DefenitionParser = 
    let private expression  = ExpressionParser.Expression
    let private typeName    = TypeParser.parser
    let private name        = TypeParser.name
    let private codeblock   = CommnadParser.codeblock
    let private keyword     = Indentation.token<<Token.Op

    let globalVar = 
        Indentation.Monad(){
            let! t = typeName
            let! e = expression
            match e with
            |Application(F Operator.Assign,_)                                   -> return Defenition.GlobalVar(t,e)
            |Application(F Operator.Comma,Application(F Operator.Assign,_)::_)  -> return Defenition.GlobalVar(t,e)
            |_                                                                  -> return! Parser.fail
        }|>Indentation.indentedScope (Token.Open "(") (Token.Close "(")
    
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
            let! f = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any name)
            return Defenition.Structdef(n,f)
        }
    let unionDefenition = 
        Indentation.Monad(){
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any name)
            return Defenition.Uniondef(n,f)
        }
    let typedef = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! t = typeName
            return Defenition.Typedef(n,t)
        }
    let typedefStruct = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "struct"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any name)
            return Defenition.File[Defenition.Structdef(n,f);Defenition.Typedef(n,Type.Struct n)]
        }
    let typedefUnion = 
        Indentation.Monad(){
            let! _ = keyword "typedef"
            let! _ = keyword "union"
            let! n = Tokenisation.word
            let! _ = keyword "="
            let! f = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any name)
            return Defenition.File[Defenition.Uniondef(n,f);Defenition.Typedef(n,Type.Union n)]
        }
    
    
    let defention = 
        [
            lazy(structDefenition)
            lazy(unionDefenition)
            lazy(typedefStruct)
            lazy(typedefUnion)
            lazy(typedef)
            lazy(functionDefention)
            lazy(functionDeclaration)
            lazy(globalVar)
        ]
        |>Parser.choose
    let file = 
        Defenition.File<^>Indentation.any defention