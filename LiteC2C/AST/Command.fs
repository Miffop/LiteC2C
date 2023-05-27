namespace LiteC2C.AST

module CommandTranslation = 
    let private expression x = Translation.final ExpressionTranslation.expression x
    let private typeName x = Translation.final TypeTranslation.typeName x
    
    let storageClass s = 
        match s with
        |StorageClass.Auto      -> Text"auto"
        |StorageClass.Register  -> Text"register"
        |StorageClass.Extern    -> Text"extern"
        |StorageClass.Static    -> Text"static"
    let declaration (d:Declaration) =   
        let declaration = [typeName d.Type;Text" ";expression d.Expression]
        match d.StorageClassOption with
        |None       -> Mix declaration
        |Some(s)    -> Mix(Translation.final storageClass s::Text " "::declaration)
    let command = 
        
        let appendSemicolon x =
            match x with
            |Command.Computation(_) ->[Data x;Text";\n"]
            |Command.LocalVar(_)    ->[Data x;Text";\n"]
            |Command.Return(_)      ->[Data x;Text";\n"]
            |Command.Goto(_)        ->[Data x;Text";\n"]
            |_                      ->[Data x;Text"\n"]
        
        let unrollSwitch cases = 
            let unrollCase (a,c) = 
                Text"case "::expression a::Text":\n"::List.collect appendSemicolon c
            List.collect unrollCase cases
            
        function
        |Command.Computation(e)                 -> Mix[expression e]
        |Command.LocalVar(d)                    -> Mix[Translation.final declaration d]
        |Command.Codeblock(commands)            -> Mix([Text"{\n"]@ List.collect appendSemicolon commands @ [Text"}"])
        |Command.WhileLoop(cond,body)           -> Mix[Text"while(";expression cond;Text")";Data body]
        |Command.DoWhileLoop(body,cond)         -> Mix[Text"do\n";Data body;Text"while(";expression cond;Text");\n"]
        |Command.ForLoop(init,cond,inc,body)    -> Mix[Text"for(";Data init;Text";";expression cond;Text";";expression inc;Text")";Data body]
        |Command.IfThenElse(cond,a,Command.Nope)-> Mix[Text"if(";expression cond;Text")";Data a]
        |Command.IfThenElse(cond,a,Command.IfThenElse(x,y,z))
                                                -> Mix[Text"if(";expression cond;Text")";Data a;Text"else ";Data(Command.IfThenElse(x,y,z))]
        |Command.IfThenElse(cond,a,b)           -> Mix[Text"if(";expression cond;Text")";Data a;Text"else";Data b]
        |Command.Return(e)                      -> Mix[Text"return ";expression e]
        |Command.SwitchCase(a,cases)            -> Mix([Text"switch(";expression a;Text"){\n"]@unrollSwitch cases@[Text"}"])
        |Command.Goto(l)                        -> Text(sprintf "goto %s" l)
        |Command.Label(l)                       -> Text(sprintf "%s:" l)
        |Command.Nope                           -> Error("'Nope' command leak")
    
    let format (s:string) = 
        let countToken x s = 
            s
            |>String.filter((=)x)
            |>String.length
        let countBrace o c s = 
            countToken o s - countToken c s
        let lines = 
            s.Split('\n')
        lines
        |>Seq.ofArray
        |>Seq.scan(fun a b -> a+countBrace '{' '}' b)0
        |>Seq.zip lines
        |>Seq.map(fun (x,y) -> String.replicate (if x.StartsWith("}") then y - 1 else y) "   " + x + "\n")
        |>Seq.reduce(+)

            
            
        