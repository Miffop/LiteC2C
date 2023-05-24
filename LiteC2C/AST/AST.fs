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
    |GlobalVar      of Name*Expression
    |Typedef        of Name*Type
    |Functiondef    of FunctionSignature
    |Function       of FunctionSignature*Command
and [<RequireQualifiedAccess>] Command = 
    |LocalVar       of Type*Expression
    |IfThenElse     of Expression*Command*Command
    |WhileLoop      of Expression*Command
    |DoWhileLoop    of Command*Expression
    |ForLoop        of Command*Expression*Expression*Command
    |Codeblock      of Command list
    |Computation    of Expression

and [<RequireQualifiedAccess>] Operator =
    //2
    |IncPost        = 0x0100
    |DecPost        = 0x0101
    |Index          = 0x0102
    |StructDeref    = 0x0103
    |StructRef      = 0x0104
    |Call           = 0x0105
    //3
    |IncPre         = 0x0200
    |DecPre         = 0x0201
    |Neg            = 0x0202
    |Not            = 0x0203
    |BitNot         = 0x0204
    |CastType       = 0x0205
    |Deref          = 0x0206
    |Ref            = 0x0207
    |SizeOf         = 0x0208
    //5
    |Mul            = 0x0300
    |Div            = 0x0301
    |Mod            = 0x0302
    //6
    |Add            = 0x0400
    |Sub            = 0x0401
    //7
    |Shl            = 0x0500
    |Shr            = 0x0501
    //9
    |Grt            = 0x0600
    |Lss            = 0x0601
    |Geq            = 0x0602
    |Leq            = 0x0603
    //10
    |Eql            = 0x0700
    |Neq            = 0x0701
    //11
    |BitAnd         = 0x0800
    //12
    |Xor            = 0x0900
    //13
    |BitOr          = 0x0A00
    //14
    |And            = 0x0B00
    //15
    |Or             = 0x0C00
    //17
    |Ternary        = 0x0D00
    |Assign         = 0x0D01
    |AssignAdd      = 0x0D02
    |AssignSub      = 0x0D03
    |AssignMul      = 0x0D04
    |AssignDiv      = 0x0D05
    |AssignMod      = 0x0D06
    |AssignAnd      = 0x0D07
    |AssignXor      = 0x0D08
    |AssignOr       = 0x0D09
    |AssignShr      = 0x0D0A
    |AssignShl      = 0x0D0B
    //18
    |Comma          = 0x0E00


and [<RequireQualifiedAccess>] Literal = 
    |Int            of int
    |Float          of float
    |Char           of string
    |String         of string
    |TypeName       of Type

and Expression = 
    |Application    of Expression*Expression list
    |F              of Operator
    |L              of Literal
    |Var            of string
    |Error          of string