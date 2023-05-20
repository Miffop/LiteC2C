namespace LiteC2C

type Parser<'s,'t> = 
    |P of ('t list->('s*'t list)option)
