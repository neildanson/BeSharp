[<AutoOpen>]
module ROP

type 'a Result = 
| Success of 'a
| Failure of string

type ROPBuilder() = 
    member __.Bind (v, f) = 
        match v with 
        | Success (a) -> f a
        | Failure message -> Failure (sprintf "%s" message)
    member __.Return x = Success x

let rop = ROPBuilder()