namespace LiteC2C.Parser

type Parser<'s,'t> = 
    |P of ('t list->('s*'t list)option)


type OptionMonad() = 
    member this.Return x = Some x
    member this.ReturnFrom x = x
    member this.Bind(Mx,f) = Option.bind f Mx

module Parser = 
    let run (P p) x = 
        p x
    let ret x = P(fun t->Some(x,t))
    let fail = P(fun _->None)
    let bind f Mx = P(fun t->
        OptionMonad(){
            let! x,t1 = run Mx t
            return! run (f x) t1
        }
    )
    type Monad() = 
        member this.Return x = ret x
        member this.ReturnFrom x = x
        member this.Bind(Mx,f) = bind f Mx

    [<AutoOpen>]
    module Ops = 
        let (<*>) p1 p2 = 
            Monad(){
                let! f = p1
                let! x = p2
                return (f x)
            }
        let (<|>) p1 (p2:Parser<'s,'t> Lazy) = P(fun t->
            match run p1 t with
            |None->run p2.Value t
            |x->x
        )
        let (<^>) f p = 
            ret f<*>p
        let (<* ) p q = (fun x _->x)<^>p<*>q
        let ( *>) p q = (fun _ x->x)<^>p<*>q

    
    let choose ls =
        List.fold(<|>)fail ls
    
    let any p = P(fun t->
        let mutable ts = t
        let s = List.unfold(fun t->ts<-t;run p t)t
        Some(s,ts)
    )
    let some p = 
        Monad(){
            let! x = p
            let! xs = any p
            return x::xs
        }
    let satisfy c = P(
        function
        |t::ts when c t -> Some(t,ts)
        |_->None
    )
    let token t = satisfy((=)t)
    let one = P(function t::ts->Some(t,ts) | []->None)

[<CustomEquality>]
[<CustomComparison>]
type Position = 
    {
        Margin:int
        Offset:int
    }
    interface System.IComparable with
        member this.CompareTo x = 
            match x with
            | :?Position as p -> 
                match this.Margin - p.Margin with
                |0 -> this.Offset - p.Offset
                |x -> x
            |_->failwithf "позицию можно сравнивать только с собой"
    override this.Equals x = 
        match x with
        | :?Position as p -> this.Margin = p.Margin && this.Offset = p.Offset
        |_ -> false
    override this.GetHashCode () = 
        this.Margin +  this.Offset



module Indentation =
    open Parser.Ops

    let line indentationToken resetToken = 
        Parser.Monad(){
            let! indentations = Parser.any(Parser.token indentationToken)
            let! restOfTheLine = Parser.any(Parser.satisfy((<>)resetToken))
            let! _ = Parser.token resetToken
            let indentation = List.length indentations
            return List.mapi(fun i t->t,{Margin = indentation;Offset = i})restOfTheLine
        }
    let text indentationToken resetToken =
        List.concat<^>Parser.any(line indentationToken resetToken)

    let private position:Parser<Position,'t*Position> = P(
        function
        |(t,p)::tp->Some(p,(t,p)::tp)
        |[]->Some({Margin = -1;Offset = 0},[])
    )
    let ret x = 
        Parser.Monad(){
            let! p = position
            return (x,p)
        }
    let bind f Mx : Parser<'b*Position,'t*Position> =  
        Parser.Monad(){
            let! (x,pos) = Mx
            let! (y,_) = f x
            return y,pos
        }
    type Monad() = 
        member this.Return x = ret x
        member this.ReturnFrom x = x
        member this.Bind(Mx,f) = bind f Mx
    [<AutoOpen>]
    module Ops =
        let (<*>) p1 p2 = 
            Monad(){
                let! f = p1 
                let! x = p2
                return f x
            }
        let (<.>) p1 (p2:Parser<'a,'t>Lazy) = 
            Monad(){
                let! f = p1 
                let! x = p2.Value
                return f x
            }
        
        let (<|>) = (<|>)
        let (<^>) f p = ret f <*> p
        let (<^ ) f p = (fun _ -> f)<^>p
        let (<* ) p q = (fun x _ -> x)<^>p<*>q
        let ( *>) p q = (fun _ x -> x)<^>p<*>q
        let (<. ) p q = (fun x _ -> x)<^>p<.>q
        let ( .>) p q = (fun _ x -> x)<^>p<.>q
        let (<**>) p q = (fun x y -> y x)<^>p<*>q
        let (<??>) p q = p<**>(q<|>lazy(ret id))

        
    let satisfy c : Parser<'t*Position,'t*Position> =
        Parser.satisfy(fst>>c)
    let token t =
        satisfy((=)t)
    
    let any p = 
        Parser.Monad(){
            let! pos = position
            let! x = Parser.any p
            return List.map fst x,pos
        }
    let some p = 
        Parser.Monad(){
            let! pos = position
            let! x = Parser.some p
            return List.map fst x,pos
        }
    
    let flip f x y = f y x
    let rec applyAll x fs =
        match fs with
        |f::fs -> applyAll (f x) fs
        |[]->x
    let chain op p = 
        applyAll<^>p<*>any(flip<^>op<*>p)
    let rec chainBack op p = 
        p<??>(flip<^>op<.>lazy(chainBack op p))

    let chainPrefix op p =  
        List.rev>>flip applyAll<^>any(op)<*>p
    let chainPostfix op p = 
        p<??>(flip applyAll<^>any(op))
    
    module Patrial = 
        let chain op p = 
            applyAll<^>p<*>some(flip<^>op<*>p)
        let chainBack op p = 
            p<**>(flip<^>op<.>lazy(chainBack op p))
        let chainPrefix op p =  
            List.rev>>flip applyAll<^>some(op)<*>p
        let chainPostfix op p = 
            p<**>(flip applyAll<^>some(op))
    

    let prefix pref p = token pref *> p 
    let prefixLazy pref p = token pref .> p
    let pack o p c = token o *> p  <* token c
    let packLazy o p c = token o .> p <* token c

    let rec pattern s = 
        match s with
        |s::[] -> (fun x->x::[])<^>token s
        |s::ss -> (fun x y->x::y)<^>token s<*>pattern ss
        |[]    -> Parser.fail

    let sameOrIndentedScope openToken closeToken p = P(
        function
        |(t,refp)::ts -> 
            let rec filter d r ts0=
                match ts0 with
                |(t,p)::ts when t = openToken && (d<>0 || p>=refp) -> filter (d+1) ((t,p)::r) ts
                |(t,p)::ts when t = closeToken && d = 0-> r,ts0
                |(t,p)::ts when t = closeToken -> filter (d-1) ((t,p)::r) ts
                |(t,p)::ts when d <> 0 || p>=refp -> filter d ((t,p)::r) ts
                
                |_->r,ts0
            let ts,rest = 
                (t,refp)::ts
                |>filter 0 []
               
            ts
            |>List.rev
            |>Parser.run p
            |>Option.map(fun (x,ts)->x,ts@rest)
        |[] -> Parser.run p []
    )

    let bindOption f p = 
        Monad(){
            let! x = p
            match f x with
            |Some(x)->return x
            |None ->return! Parser.fail
        }
    let mapTokens (m:Map<'a,'b>) = 
        (fun x->m[x])<^>satisfy(flip Map.containsKey m)

        