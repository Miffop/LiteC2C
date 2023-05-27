namespace LiteC2C.AST

module DefenitionTranslation = 
    let defention = 
        let expression x = Translation.final ExpressionTranslation.expression x
        let command x = Translation.final CommandTranslation.command x
        let typeName x = Translation.final TypeTranslation.typeName x
        let name x = Translation.final TypeTranslation.name x
        let declaration x = Translation.final CommandTranslation.declaration x
        let arrow n x = TypeTranslation.arrowFinal n x

        let functionSignature (signature:FunctionSignature) = 
            let parameters = 
                match signature.Parameters with
                |p::ps -> name p::List.collect(fun x->[Text",";name x]) ps
                |[] -> []
            Mix([name signature.Name;Text"("] @ parameters @ [Text")"])
        
        function
        |Defenition.GlobalVar(d)                -> Mix[declaration d;Text";"]
        |Defenition.Function(signature)         -> Mix[functionSignature signature;Text";"]
        |Defenition.Functiondef(signature,code) -> Mix[functionSignature signature;command code]
        |Defenition.File(defs)                  -> defs|>List.collect(function Defenition.File f -> [Data (Defenition.File f)] | d->[Data d;Text"\n"])|>Mix
        |Defenition.Structdef(n,f)              -> Mix([Text("struct "+n+"{\n")]@ List.collect(fun x->[name x;Text";\n"])f @[Text"};"])
        |Defenition.Uniondef(n,f)               -> Mix([Text("union "+n+"{\n")]@ List.collect(fun x->[name x;Text";\n"])f @[Text"};"])
        |Defenition.Typedef(n,t)                -> Mix([Text"typedef ";typeName t;Text(" "+n+";")])
        |Defenition.TypedefFunctionPointer(n,f) -> Mix([Text"typedef ";arrow n f;Text";"])