namespace LiteC2C.AST

type 't Translation = 
    |Text of string
    |Data of 't
    |Mix of 't Translation list
    |Error of string

module Translation = 
    
    let step translator data=
        match data with
        |Data(x) -> translator x
        |Mix(Data(x)::rest)->Mix(translator x :: rest)
        |Mix(a::Data(x)::rest)->Mix(a::translator x::rest)
        |Mix(Text x::Text y::rest) -> Mix(Text(x+y)::rest)
        |Mix(Text x::[]) -> Text x
        |Mix(Mix(a)::rest)->Mix(a@rest)
        |Mix(a::Mix(b)::rest)->Mix((a::b)@rest)
        |Mix([])->Text ""
        |Mix(Error(x)::_)->Error(x)
        |Mix(_::Error(x)::_)->Error(x)
        |x->x
    let translate translator data = 
        Seq.unfold(fun x->let nx = step translator x in if nx = x then None else Some(nx,nx))(Data data)
        |>Seq.last
    let final translator data = 
        match translate translator data with
        |Text(x)->Text(x)
        |x->Error(sprintf "cannot reduce: %A" x)
