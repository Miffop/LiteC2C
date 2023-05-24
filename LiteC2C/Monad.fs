namespace LiteC2C

type OptionMonad() = 
    member this.Return x = Some x
    member this.ReturnFrom x = x
    member this.Bind(Mx,f) = Option.bind f Mx