namespace LiteC2C.Parser

open LiteC2C.AST
open Indentation.Ops

module CommnadParser = 
    
    let localVar = 
        Indentation.Monad(){
            let! t = TypeParser.parser
            let! e = ExpressionParser.Expression
            match e with
            |Application(F Operator.Assign,_) -> return Command.LocalVar(t,e)
            |Application(F Operator.Comma,Application(F Operator.Assign,_)::_) -> return Command.LocalVar(t,e)
            |_ -> return! Parser.fail
        }|>Indentation.indentedScope (Token.Open "(") (Token.Close "(")
    let computation = 
        Command.Computation<^>Indentation.indentedScope (Token.Open "(") (Token.Close "(") ExpressionParser.Expression
    let whileLoop codeblock = 
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "while")
            let! cond = ExpressionParser.Expression
            let! _ = Indentation.token(Token.Op "do")
            let! body = codeblock
            return Command.WhileLoop(cond,body)
        }
    let doWhileLoop codeblock = 
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "do")
            let! body = codeblock
            let! _ = Indentation.token(Token.Op "while")
            let! cond = ExpressionParser.Expression
            return Command.DoWhileLoop(body,cond)
        }
    let ifThenElse codeblock = 
        let elseIf = 
            Indentation.Monad(){
                let! _ = Indentation.token(Token.Op "else")
                let! _ = Indentation.token(Token.Op "if")
                let! cond = ExpressionParser.Expression
                let! _ = Indentation.token(Token.Op "then")
                let! a = codeblock
                return (fun b -> Command.IfThenElse(cond,a,b))
            }
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "if")
            let! cond = ExpressionParser.Expression
            let! _ = Indentation.token(Token.Op "then")
            let! a = codeblock
            let! branches = Indentation.any elseIf
            let! _ = Indentation.token(Token.Op "else")
            let! b = codeblock
            return Command.IfThenElse(cond,a,List.foldBack id branches b)
        }
    let forLoop command codeblock = 
        Indentation.Monad(){
            let! _ = Indentation.token(Token.Op "for")
            let! init = command
            let! _ = Indentation.token(Token.Op ";")
            let! cond = ExpressionParser.Expression
            let! _ = Indentation.token(Token.Op ";")
            let! inc = ExpressionParser.Expression
            let! _ = Indentation.token(Token.Op "do")
            let! body = codeblock
            return Command.ForLoop(init,cond,inc,body)
        }
    let doBlock codeblock = 
        Indentation.token(Token.Op "do")*>codeblock

    let rec command = 
        [
            lazy(ifThenElse codeblock)
            lazy(whileLoop codeblock)
            lazy(doWhileLoop codeblock)
            lazy(forLoop command codeblock)
            lazy(doBlock codeblock)
            lazy(localVar)
            lazy(computation)
        ]
        |>Parser.choose
    and codeblock = 
        Command.Codeblock<^>Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") (Indentation.any command)
