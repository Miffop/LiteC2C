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
    |IncPost        = 0x0000
    |DecPost        = 0x0001
    |Index          = 0x0002
    |StructDeref    = 0x0003
    |StructRef      = 0x0004
    //3
    |IncPre         = 0x0100
    |DecPre         = 0x0101
    |Neg            = 0x0102
    |Not            = 0x0103
    |BitNot         = 0x0104
    |CastType       = 0x0105
    |Deref          = 0x0106
    |Ref            = 0x0107
    |SizeOf         = 0x0108
    //5
    |Mul            = 0x0200
    |Div            = 0x0201
    |Mod            = 0x0202
    //6
    |Add            = 0x0300
    |Sub            = 0x0301
    //7
    |Shl            = 0x0400
    |Shr            = 0x0401
    //9
    |Grt            = 0x0500
    |Lss            = 0x0501
    |Geq            = 0x0502
    |Leq            = 0x0503
    //10
    |Eql            = 0x0600
    |Neq            = 0x0601
    //11
    |BitAnd         = 0x0700
    //12
    |Xor            = 0x0800
    //13
    |BitOr          = 0x0900
    //14
    |And            = 0x0A00
    //15
    |Or             = 0x0B00
    //17
    |Ternary        = 0x0C00
    |Assign         = 0x0C01
    |AssignAdd      = 0x0C02
    |AssignSub      = 0x0C03
    |AssignMul      = 0x0C04
    |AssignDiv      = 0x0C05
    |AssignMod      = 0x0C06
    |AssignAnd      = 0x0C07
    |AssignXor      = 0x0C08
    |AssignOr       = 0x0C09
    |AssignShr      = 0x0C0A
    |AssignShl      = 0x0C0B
    //18
    |Comma          = 0x0D00


and [<RequireQualifiedAccess>] Literal = 
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
    |Error of string