namespace LiteC2C.Parser

open LiteC2C.AST
open Indentation.Ops

module CommnadParser = 
    let private keyword x = Indentation.token(Token.Op x)
    let private expression = ExpressionParser.Expression
    let private typeName = TypeParser.parser
    let private word = Indentation.bindOption(function Token.Word x -> Some x | _ -> None)Parser.one

    let storageClass = 
        [
            lazy(StorageClass.Auto <^ keyword "auto")
            lazy(StorageClass.Register <^ keyword "register")
            lazy(StorageClass.Extern <^ keyword "extern")
            lazy(StorageClass.Static <^ keyword "static")
        ]
        |>Parser.choose

    let declaration = 
        Indentation.Monad(){
            let! storageClassOption = Indentation.option storageClass
            let! typeName = typeName
            let! expression = expression
            match expression with
            |Application(F Operator.Assign,_)                                   
            |Application(F Operator.Comma,Application(F Operator.Assign,_)::_)  
            |Var(_)                                                             
            |Application(F Operator.Comma,Var(_)::_)                            -> return {StorageClassOption = storageClassOption; Type = typeName; Expression = expression}
            |_                                                                  -> return! Parser.fail
        }|>Indentation.indentedScope (Token.Open "(") (Token.Close "(")

    let localVar = 
        Command.LocalVar<^>declaration

    let computation = 
        Command.Computation<^>Indentation.indentedScope (Token.Open "(") (Token.Close "(") ExpressionParser.Expression
    
    let whileLoop codeblock = 
        Indentation.Monad(){
            let! _ = keyword "while"
            let! cond = expression
            let! _ = keyword "do"
            let! body = codeblock
            return Command.WhileLoop(cond,body)
        }
    
    let doWhileLoop codeblock = 
        Indentation.Monad(){
            let! _ = keyword "do"
            let! body = codeblock
            let! _ = keyword "while"
            let! cond = expression
            return Command.DoWhileLoop(body,cond)
        }
    
    let ifThenElse codeblock = 
        let elseIf = 
            Indentation.Monad(){
                let! _ = keyword "else"
                let! _ = keyword "if"
                let! cond = expression
                let! _ = keyword "then"
                let! a = codeblock
                return fun b -> Command.IfThenElse(cond,a,b)
            }
        Indentation.Monad(){
            let! _ = keyword "if"
            let! cond = expression
            let! _ = keyword "then"
            let! a = codeblock
            let! branches = Indentation.any elseIf
            let! b = Indentation.option(keyword "else" *> codeblock)
            return Command.IfThenElse(cond,a,List.foldBack id branches (Option.defaultValue Command.Nope b))
        }
    
    let forLoop command codeblock = 
        Indentation.Monad(){
            let! _ = keyword "for"
            let! init = command
            let! _ = keyword ";"
            let! cond = expression
            let! _ = keyword ";"
            let! inc = expression
            let! _ = keyword "do"
            let! body = codeblock
            return Command.ForLoop(init,cond,inc,body)
        }
    
    let doBlock codeblock = 
        keyword "do"*>codeblock
    
    let returnStatement =
        Command.Return<^keyword "return"<*>ExpressionParser.Expression
    
    let breakContinueGoto = 
        [
            lazy(Command.Computation(Var"break")<^keyword"break")
            lazy(Command.Computation(Var"continue")<^keyword"continue")
            lazy(Command.Goto<^keyword"goto"<*>word)
            lazy(Command.Label<^keyword"label"<*>word)
        ]
        |>Parser.choose

    let switchCase command =
        let caseDo = 
            Indentation.Monad(){
                let! _ = keyword "case"
                let! e = expression
                let! _ = keyword "do"
                let! c = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any command)
                return e,c
            }
        Indentation.Monad(){
            let! _ = keyword "switch"
            let! a = ExpressionParser.Expression
            let! _ = keyword "of"
            let! c = Indentation.some caseDo
            return Command.SwitchCase(a,c)
        }

    let rec command = 
        [
            lazy(ifThenElse codeblock)
            lazy(whileLoop codeblock)
            lazy(doWhileLoop codeblock)
            lazy(forLoop command codeblock)
            lazy(doBlock codeblock)
            lazy(switchCase command)
            lazy(returnStatement)
            lazy(breakContinueGoto)
            lazy(localVar)
            lazy(computation)
        ]
        |>Parser.choose
    
    and codeblock = 
        Command.Codeblock<^>Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any command)
