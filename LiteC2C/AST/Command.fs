namespace LiteC2C.AST

module CommandTranslation = 
    let command = 
        let expression = Translation.final ExpressionTranslation.expression
        let typeName = Translation.final TypeTranslation.typeName
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
        |Command.LocalVar(t,e)                  -> Mix[typeName t;Text" ";expression e]
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