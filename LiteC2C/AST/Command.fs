namespace LiteC2C.AST

module CommandTranslation = 
    let command = 
        let expression = Translation.final ExpressionTranslation.expression
        let typeName = Translation.final TypeTranslation.typeName
        let appendSemicolon x =
            match x with
            |Command.Computation(_) ->[Data x;Text";\n"]
            |Command.LocalVar(_,_)  ->[Data x;Text";\n"]
            |_                      ->[Data x;Text"\n"]
            
        function
        |Command.Computation(e)                 -> Mix[expression e]
        |Command.LocalVar(t,e)                  -> Mix[typeName t;Text" ";expression e]
        |Command.Codeblock(commands)            -> Mix([Text"{\n"]@ List.collect appendSemicolon commands @ [Text"}"])
        |Command.WhileLoop(cond,body)           -> Mix[Text"while(";expression cond;Text")";Data body]
        |Command.DoWhileLoop(body,cond)         -> Mix[Text"do\n";Data body;Text"while(";expression cond;Text");\n"]
        |Command.ForLoop(init,cond,inc,body)    -> Mix[Text"for(";Data init;Text";";expression cond;Text";";expression inc;Text")";Data body]
        |Command.IfThenElse(cond,a,b)           -> Mix[Text"if(";expression cond;Text")";Data a;Text"else";Data b]