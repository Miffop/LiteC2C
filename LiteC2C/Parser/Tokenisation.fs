namespace LiteC2C.Parser

[<RequireQualifiedAccess>]
type Token = 
    |String of string
    |Char of string
    |Int of int
    |Float of float
    |Word of string
    |Op of string
    |Open of string
    |Close of string

module Tokenisation = 
    open Indentation.Ops
    let stringToken,charToken = 
        let char = 
            [
                lazy(sprintf"%c%c"<^>Indentation.token '\\'<*>Parser.one)
                lazy(sprintf"%c"<^>Indentation.satisfy((<>)'"'))
            ]
            |>Parser.choose
        List.reduce(+)>>Token.String<^>Indentation.pack '"' (Indentation.any char) '"',
        Token.Char<^>Indentation.pack '\'' (char) '\''
    let inline sign p = 
        [
            lazy(Indentation.Monad(){
                let! s = Indentation.token '-' <|> lazy(Indentation.token '+')
                let! x = p
                match s with
                |'+'->return x
                |'-'->return -x
                |_  ->return! Parser.fail
            })
            lazy(p)
        ]
        |>Parser.choose
    let intToken =
        let uint = 
            List.fold(sprintf"%s%c")"">>int<^>Indentation.some(Indentation.satisfy System.Char.IsDigit)
        Token.Int<^>sign uint
    let floatToken = 
        let ufloat = 
            (fun x d z->x@(d::z))<^>Indentation.some(Indentation.satisfy System.Char.IsDigit)<*>Indentation.token '.'<*>Indentation.any(Indentation.satisfy System.Char.IsDigit)
            |>(<^>)(List.fold(sprintf"%s%c")"">>float)
        Token.Float<^>sign ufloat
    let stringPattern(s:string)=
        (fun _->s)<^>Indentation.pattern(List.ofSeq s)
    let opToken = 
        [
            //arithmetic
            lazy(stringPattern("++"))
            lazy(stringPattern("--"))
            lazy(stringPattern("+"))
            lazy(stringPattern("-"))
            lazy(stringPattern("*"))
            lazy(stringPattern("/"))
            lazy(stringPattern("%"))
            //bitshifts
            lazy(stringPattern(">>"))
            lazy(stringPattern("<<"))
            //comparison
            lazy(stringPattern("=="))
            lazy(stringPattern("!="))
            lazy(stringPattern(">="))
            lazy(stringPattern("<="))
            lazy(stringPattern(">"))
            lazy(stringPattern("<"))
            //logic
            lazy(stringPattern("!"))
            lazy(stringPattern("&&"))
            lazy(stringPattern("||"))
            //bitwise
            lazy(stringPattern("~"))
            lazy(stringPattern("&"))
            lazy(stringPattern("|"))
            lazy(stringPattern("^"))
            //poiters
            lazy(stringPattern("."))
            lazy(stringPattern("->"))
            //evaluation
            lazy(stringPattern("="))
            lazy(stringPattern(","))
            lazy(stringPattern(";"))
            lazy(stringPattern("?"))
            lazy(stringPattern(":"))
        ]
        |>Parser.choose
        |>(<^>)Token.Op
    


    let wordToken = 
        let opOrWord = 
            let ops = 
                [
                    "sizeof"
                ]
            function 
            |x when List.contains x ops -> Token.Op x
            |x -> Token.Word x
        Indentation.Monad(){
            let! x = Indentation.satisfy System.Char.IsLetter
            let! xs = Indentation.any(Indentation.satisfy System.Char.IsLetterOrDigit)
            return opOrWord<|List.fold(sprintf"%s%c")""(x::xs)
        }

    let braceToken = 
        [
            '(',    Token.Open"("
            '[',    Token.Open"["
            '{',    Token.Open"{"
            ')',    Token.Close"("
            ']',    Token.Close"["
            '}',    Token.Close"{"
        ]
        |>Map.ofList
        |>Indentation.mapTokens
    let AllTokens = 
        [
            lazy(floatToken)
            lazy(intToken)

            lazy(stringToken)
            lazy(charToken)

            lazy(braceToken)
            lazy(opToken)
            lazy(wordToken)
        ]
        |>Parser.choose
    open Parser.Ops
    let Tokenizer = 
        let Void = Indentation.any(Indentation.satisfy System.Char.IsWhiteSpace)
        Void*>Parser.any(AllTokens<*Void)
        