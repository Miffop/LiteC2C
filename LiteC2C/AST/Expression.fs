namespace LiteC2C.AST

module TypeTranslation = 
    let typeName = 
        let name (n:Name) = 
            [Data(n.Type);Text(sprintf " %s;" n.Name)]
        function
        |Type.Custom        x -> Text(sprintf "%s" x)
        |Type.Pointer       x -> Mix[Data x;Text "*"]
        |Type.Struct        x -> Text("struct " + x)
        |Type.Union         x -> Text("union " + x)

    let name (n:Name) = 
        Mix[Translation.final typeName n.Type;Text(" " + n.Name)]

    
    module Arrow = 
        type arrowTypeAsList = 
            {
                Result:Type
                ArgumentsFliped:ArrowOrType list list
            }
        type arrowWithName = 
            {
                Name:string
                Arrow:ArrowType
            }
        let rec arrow2ArrowAsList argumentsFliped (a:ArrowType) = 
            match a.Result with
            |ArrowOrType.Type x -> { Result = x; ArgumentsFliped = a.Arguments::argumentsFliped }
            |ArrowOrType.Arrow x -> arrow2ArrowAsList (a.Arguments::argumentsFliped) x
        
        let translator (a:arrowWithName) =
            let arrowOrType =
                function
                |ArrowOrType.Arrow f -> Data { Arrow = f; Name = ""}
                |ArrowOrType.Type t  -> Translation.final typeName t
            let arguments =
                function
                |[]     -> [Text(")()")]
                |x::y   -> [Text")(";arrowOrType x;Mix(List.collect(fun x -> [Text",";arrowOrType x])y);Text")"]
            let arrowAsList = 
                arrow2ArrowAsList [] a.Arrow
            let stars = 
                List.replicate (List.length arrowAsList.ArgumentsFliped) (Text"(*")
            let args = 
                List.collect(arguments)<|List.rev arrowAsList.ArgumentsFliped
            Mix[Translation.final typeName arrowAsList.Result;Mix(stars);Text a.Name;Mix(args)]
    let arrowFinal (name:string) (a:ArrowType) =
        Translation.final Arrow.translator { Arrow.Arrow = a; Arrow.Name = name }
        

module ExpressionTranslation = 
    let literal = 
        function
        |Literal.Int      x -> Text<|sprintf "%i"     x
        |Literal.Float    x -> Text<|sprintf "%ff"    x
        |Literal.Char     x -> Text<|sprintf "'%s'"   x
        |Literal.String   x -> Text<|sprintf "\"%s\"" x
        |Literal.TypeName x -> Mix[Text"(";Translation.final TypeTranslation.typeName x;Text")"]

    let PrecedenceUnary ref a = 
        match a with
        |Application(F x,y) when (int x &&& 0xFF00) > (int ref &&& 0xFF00) -> Mix[Text"(";Data a;Text")"]
        |x->Data x

    let private PrecedenceGeq ref a = 
        match a with
        |Application(F x,y) when (int x &&& 0xFF00) >= (int ref &&& 0xFF00) -> Mix[Text"(";Data a;Text")"]
        |x->Data x

    let BinaryOpLeft ref op a b =
        let q = ref
        Mix[PrecedenceUnary ref a;Text op;PrecedenceGeq ref b]

    let BinaryOpRight ref op a b =
        Mix[PrecedenceGeq ref a;Text op;PrecedenceUnary ref b]

    let expression =
        function            
        //2
        |Application(F Operator.IncPost,    a::[])      -> Mix[PrecedenceUnary(Operator.IncPost) a;Text "++"]
        |Application(F Operator.DecPost,    a::[])      -> Mix[PrecedenceUnary(Operator.DecPost) a;Text "--"]
        |Application(F Operator.Index,      a::b::[])   -> Mix[PrecedenceUnary(Operator.Index) a;Text "[";Data b;Text"]"]
        |Application(F Operator.StructDeref,a::b::[])   -> BinaryOpLeft Operator.StructDeref    "->"    a b
        |Application(F Operator.StructRef,  a::b::[])   -> BinaryOpLeft Operator.StructRef      "."     a b
        //3
        |Application(F Operator.IncPre,     a::[])      -> Mix[Text"++";PrecedenceUnary(Operator.IncPre)a]
        |Application(F Operator.DecPre,     a::[])      -> Mix[Text"--";PrecedenceUnary(Operator.DecPre)a]
        |Application(F Operator.Neg,        a::[])      -> Mix[Text"-";PrecedenceUnary(Operator.Neg)a]
        |Application(F Operator.Not,        a::[])      -> Mix[Text"!";PrecedenceUnary(Operator.Not)a]
        |Application(F Operator.BitNot,     a::[])      -> Mix[Text"~";PrecedenceUnary(Operator.BitNot)a]
        |Application(F Operator.Ref,        a::[])      -> Mix[Text"&";PrecedenceUnary(Operator.Ref)a]
        |Application(F Operator.Deref,      a::[])      -> Mix[Text"*";PrecedenceUnary(Operator.Deref)a]
        |Application(F Operator.CastType,   a::b::[])   -> Mix[Data a;PrecedenceUnary(Operator.CastType)b]
        |Application(F Operator.SizeOf,     L(Literal.TypeName t)::[])
                                                        -> Mix[Text"sizeof";Data(L<|Literal.TypeName t)]
        |Application(F Operator.SizeOf,     a::[])      -> Mix[Text"sizeof ";PrecedenceUnary(Operator.SizeOf)a]
        //5
        |Application(F Operator.Mul,        a::b::[])   -> BinaryOpLeft Operator.Mul        "*"     a b
        |Application(F Operator.Div,        a::b::[])   -> BinaryOpLeft Operator.Div        "/"     a b
        |Application(F Operator.Mod,        a::b::[])   -> BinaryOpLeft Operator.Mod        "%"     a b
        //6
        |Application(F Operator.Add,        a::b::[])   -> BinaryOpLeft Operator.Add        "+"     a b
        |Application(F Operator.Sub,        a::b::[])   -> BinaryOpLeft Operator.Sub        "-"     a b
        //7
        |Application(F Operator.Shr,        a::b::[])   -> BinaryOpLeft Operator.Shr        ">>"    a b
        |Application(F Operator.Shl,        a::b::[])   -> BinaryOpLeft Operator.Shl        "<<"    a b
        //9
        |Application(F Operator.Grt,        a::b::[])   -> BinaryOpLeft Operator.Grt        ">"     a b
        |Application(F Operator.Lss,        a::b::[])   -> BinaryOpLeft Operator.Lss        "<"     a b
        |Application(F Operator.Geq,        a::b::[])   -> BinaryOpLeft Operator.Geq        ">="    a b
        |Application(F Operator.Leq,        a::b::[])   -> BinaryOpLeft Operator.Leq        "<="    a b
        //10
        |Application(F Operator.Eql,        a::b::[])   -> BinaryOpLeft Operator.Eql        "=="    a b
        |Application(F Operator.Neq,        a::b::[])   -> BinaryOpLeft Operator.Neq        "!="    a b
        //11
        |Application(F Operator.BitAnd,     a::b::[])   -> BinaryOpLeft Operator.BitAnd     "&"     a b
        //12
        |Application(F Operator.Xor,        a::b::[])   -> BinaryOpLeft Operator.Xor        "^"     a b
        //13
        |Application(F Operator.BitOr,      a::b::[])   -> BinaryOpLeft Operator.BitOr      "|"     a b
        //14
        |Application(F Operator.And,        a::b::[])   -> BinaryOpLeft Operator.And        "&&"    a b
        //15
        |Application(F Operator.Or,         a::b::[])   -> BinaryOpLeft Operator.Or         "||"    a b
        //17
        |Application(F Operator.Assign,     a::b::[])   -> BinaryOpRight Operator.Assign    "="     a b
        |Application(F Operator.AssignAdd,  a::b::[])   -> BinaryOpRight Operator.AssignAdd "+="    a b
        |Application(F Operator.AssignSub,  a::b::[])   -> BinaryOpRight Operator.AssignSub "-="    a b
        |Application(F Operator.AssignMul,  a::b::[])   -> BinaryOpRight Operator.AssignMul "*="    a b
        |Application(F Operator.AssignDiv,  a::b::[])   -> BinaryOpRight Operator.AssignDiv "/="    a b
        |Application(F Operator.AssignMod,  a::b::[])   -> BinaryOpRight Operator.AssignMod "%="    a b
        |Application(F Operator.AssignShr,  a::b::[])   -> BinaryOpRight Operator.AssignShr ">>="   a b
        |Application(F Operator.AssignShl,  a::b::[])   -> BinaryOpRight Operator.AssignShl "<<="   a b
        |Application(F Operator.AssignAnd,  a::b::[])   -> BinaryOpRight Operator.AssignAnd "&="    a b
        |Application(F Operator.AssignXor,  a::b::[])   -> BinaryOpRight Operator.AssignXor "^="    a b
        |Application(F Operator.AssignOr,   a::b::[])   -> BinaryOpRight Operator.AssignOr  "|="    a b
        //17
        |Application(F Operator.Ternary,    a::b::c::[])-> Mix[PrecedenceGeq(Operator.Ternary)a;Text "?";Data b;Text":";PrecedenceUnary(Operator.Ternary)c]
        //18
        |Application(F Operator.Comma,      a::b::[])   -> BinaryOpLeft Operator.Comma      ","     a b
        //1
        |Application(f,p::ps)                           -> let mapArgument = 
                                                                function
                                                                |Application(F Operator.Comma,x)->Mix[Text"(";Data<|Application(F Operator.Comma,x);Text")"]
                                                                |x->Data x
                                                           let parameters =
                                                                let foldBack f a l = List.foldBack f l a
                                                                ps
                                                                |>List.rev
                                                                |>List.map(mapArgument)
                                                                |>foldBack(fun x y->x::Text","::y)[]
                                                                |>List.rev
                                                                |>Mix
                                                           Mix[PrecedenceUnary(Operator.Call)f;Text"(";mapArgument p;parameters;Text")"]
        |Application(f,[])                              -> Mix[PrecedenceUnary(Operator.Call)f;Text"()"]
        //Other
        |L(x) -> Translation.final literal x
        |Var(x) -> Text(x)
        |x->Data x