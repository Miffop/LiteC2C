namespace LiteC2C.AST

[<RequireQualifiedAccess>]
type Type = 
    |Custom of string
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
    //1
    //function call
    //2
    |IncPost
    |DecPost
    |Index
    |StructDeref
    |StructRef
    //3
    |IncPre
    |DecPre
    |Neg
    |Not
    |BitNot
    |CastType
    |Deref
    |Ref
    |SizeOf
    //5
    |Mul
    |Div
    |Mod
    //6
    |Add
    |Sub
    //7
    |Shl
    |Shr
    //9
    |Grt
    |Lss
    |Geq
    |Leq
    //10
    |Eql
    |Neq
    //11
    |BitAnd
    //12
    |Xor
    //13
    |BitOr
    //14
    |And
    //15
    |Or
    //17
    |Ternary
    |Assign
    |AssignAdd
    |AssignSub
    |AssignMul
    |AssignDiv
    |AssignMod
    |AssignAnd
    |AssignXor
    |AssignOr
    |AssignShr
    |AssignShl
    //18
    |Comma

and Literal = 
    |Int of int
    |Float of float
    |Char of string
    |String of string
    |TypeName of Type

and Expression = 
    |Application of Expression*Expression list
    |F of Operator
    |L of Literal
    |Var of string