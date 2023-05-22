namespace LiteC2C.Parser


open LiteC2C.AST
open Indentation.Ops

module TypeParser = 
    let customType = 
        Indentation.bindOption(function Token.Word(x)->Some(Type.Custom x) | x->None)Parser.one
    let pointerType =
        Indentation.chainPostfix(Type.Pointer<^Indentation.token(Token.Op "*"))


    let rec parser = 
        [
            lazy(pointerType customType)
        ]
        |>Parser.choose

module ExpressionParser = 
    
    let private flip f x y = f y x
    let private precedenceLayer ops =
        ops
        |>Map.ofList
        |>Indentation.mapTokens

    let (<||>) l1 l2 p = 
        let s1 x = 
            let res = l1 x
            res
        let s2 x = 
            let res = l2 x
            res
        (s1 p)<|>lazy(s2 p)
    let chooseLayer ls = 
        List.reduce(<||>)ls

    module Binary =
        let private Op op x y = Application(op,x::y::[])
        let Precedence2 = 
            [
                Token.Op ".",   Op(F Operator.StructRef)
                Token.Op "->",  Op(F Operator.StructDeref)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence5 = 
            [
                Token.Op "*",   Op(F Operator.Mul)
                Token.Op "/",   Op(F Operator.Div)
                Token.Op "%",   Op(F Operator.Mod)  
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence6 =
            [
                Token.Op "+",   Op(F Operator.Add)
                Token.Op "-",   Op(F Operator.Sub)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence7 =
            [
                Token.Op ">>",  Op(F Operator.Shr)
                Token.Op "<<",  Op(F Operator.Shl)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence9 = 
            [
                Token.Op ">",   Op(F Operator.Grt)
                Token.Op "<",   Op(F Operator.Lss)
                Token.Op ">=",  Op(F Operator.Geq)
                Token.Op "<=",  Op(F Operator.Leq)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence10 = 
            [
                Token.Op "==",  Op(F Operator.Eql)
                Token.Op "!=",  Op(F Operator.Neq)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence11 = 
            [
                Token.Op "&",   Op(F Operator.BitAnd)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence12 = 
            [
                Token.Op "^",   Op(F Operator.Xor)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence13 = 
            [
                Token.Op "|",   Op(F Operator.BitOr)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence14 = 
            [
                Token.Op "&&",  Op(F Operator.And)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence15 = 
            [
                Token.Op "||",  Op(F Operator.Or)
            ]
            |>precedenceLayer
            |>Indentation.chain
        let Precedence17 = 
            [
                Token.Op "=",   Op(F Operator.Assign)
                Token.Op "+=",  Op(F Operator.AssignAdd)
                Token.Op "-=",  Op(F Operator.AssignSub)
                Token.Op "*=",  Op(F Operator.AssignMul)
                Token.Op "/=",  Op(F Operator.AssignDiv)
                Token.Op "%=",  Op(F Operator.AssignMod)
                Token.Op "&=",  Op(F Operator.AssignAnd)
                Token.Op "^=",  Op(F Operator.AssignXor)
                Token.Op "|=",  Op(F Operator.AssignOr)
                Token.Op ">>=", Op(F Operator.AssignShr)
                Token.Op "<<=", Op(F Operator.AssignShl)
            ]
            |>precedenceLayer
            |>Indentation.chainBack
        let Precedence18 = 
            [
                Token.Op ",",   Op(F Operator.Comma)
            ]
            |>precedenceLayer
            |>Indentation.chainBack
    module Unary = 
        let private Op op x = Application(op,x::[])
        let Precedence2 x = 
            let Indexation = 
                Indentation.Monad(){
                    let! e = Indentation.packLazy (Token.Open "[") x (Token.Close "[")
                    return (fun x->Application(F Operator.Index,x::e::[]))
                }
            [
                Token.Op "++",  Op(F Operator.IncPost)
                Token.Op "--",  Op(F Operator.DecPost)
            ]
            |>precedenceLayer
            |>flip(<|>)(lazy(Indexation))
            |>Indentation.Patrial.chainPostfix
        let Precedence3 = 
            let TypeCasting = 
                Indentation.Monad(){
                    let! t = Indentation.pack (Token.Open "(") TypeParser.parser (Token.Close "(")
                    return (fun x->Application(F Operator.CastType,L(Literal.TypeName t)::x::[]))
                }
            [
                Token.Op "++",  Op(F Operator.IncPre)
                Token.Op "--",  Op(F Operator.DecPre)
                Token.Op "-",   Op(F Operator.Neg)
                Token.Op "!",   Op(F Operator.Not)
                Token.Op "~",   Op(F Operator.BitNot)
                Token.Op "*",   Op(F Operator.Deref)
                Token.Op "&",   Op(F Operator.Ref)
            ]
            |>precedenceLayer
            |>flip(<|>)(lazy(TypeCasting))
            |>Indentation.chainPrefix
    module Special = 
        let IfThenElse (x:Parser<_,_>Lazy) p = 
            let p = Indentation.sameOrIndentedScope (Token.Open "(") (Token.Close "(") p
            Indentation.Monad(){
                let! _ = Indentation.token(Token.Op "if")
                let! c = x.Value
                let! _ = Indentation.token(Token.Op "then")
                let! a = x.Value
                let! _ = Indentation.token(Token.Op "else")
                let! b = p
                return Application(F Operator.Ternary,c::a::b::[])
            }
        
        let FuctionCall p = 
            let emptyCall =
                (fun x->Application(x,[]))<^Indentation.token(Token.Op "()")
            let normalCall =
                (fun l x->Application(x,l))<^>Indentation.some(p)
            p<??>(normalCall<|>lazy(emptyCall))
    module Elem = 
        let Literal = 
            let f = 
                function
                |Token.Int(x)->     Some<|L(Literal.Int x)
                |Token.Float(x)->   Some<|L(Literal.Float x)
                |Token.String(x)->  Some<|L(Literal.String x)
                |Token.Char(x)->    Some<|L(Literal.Char x)
                |_->                None
            Indentation.bindOption f Parser.one
    let rec Expression = 
        let layers = 
            [
                Binary.Precedence18 
                Special.IfThenElse (lazy(Expression))<||>Binary.Precedence17
                Binary.Precedence15
                Binary.Precedence14
                Binary.Precedence13
                Binary.Precedence12
                Binary.Precedence11
                Binary.Precedence10
                Binary.Precedence9
                Binary.Precedence7
                Binary.Precedence6
                Binary.Precedence5
                Unary.Precedence3
                Unary.Precedence2 (lazy(Expression))<||>Binary.Precedence2
                Special.FuctionCall
            ]
        List.foldBack(fun x y->x y)layers Element
    and Element = 
        [
            lazy(Elem.Literal)
        ]
        |>Parser.choose