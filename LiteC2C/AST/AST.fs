namespace LiteC2C.AST

type Type = 
    |CustomType of string
    |Struct of Name list
    |Pointer of Type
and Name =
    {
        Name:string
        Type:Type
    }
type FunctionSignature = 
    {
        Name:Name
        Parameters:Name list
    }
and Defenitions = 
    |GlobalVar of Name*Expression
    |Typedef of Name*Type
    |Functiondef of FunctionSignature
    |Function of FunctionSignature*Codeblock
and Codeblock = Command list
and Command = 
    |LocalVar of Name*Expression
    |IfThenElse of Expression*Codeblock
    |WhileLoop of Expression*Codeblock
    |ForLoop of Command*Expression*Expression*Codeblock
    |DoBlock of Codeblock
and Operator =
    //Maths
    |Add
    |Sub
    |Mul
    |Div
    |Mod
    //Maths unary
    |Neg
    |IncPre
    |IncPost
    |DecPre
    |DecPost
    //Comparison
    |Eql
    |Neq
    |Grt
    |Lss
    |Geq
    |Leq
    //Logic
    |Not
    |And
    |Or
    //Bitwise
    |BitNot
    |BitAnd
    |BitOr
    |Xor
    |Shl
    |Shr
    //Poiters
    |Index
    |Deref
    |Ref
    |StructDeref
    |StructRef
    //Other(by eng wiki)
    |Comma
    |Ternary
    |SizeOf
    |Conversion

and Literal = 
    |Int of int
    |Float of float
    |Char of char
    |String of string
    |TypeName of Type

and Expression = 
    |Application of Expression*Expression list
    |F of Operator
    |L of Literal
    |Var of string