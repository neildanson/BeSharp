[<AutoOpen>]
module Constants

type Name = string
type TypeName = string
type MethodName = string

let ``if`` = "if"
let ``else`` = "else"
let ``struct`` = "struct"
let ``let`` = "let"
let ``func`` = "func"

let keywords = [``if``; ``else``; ``struct``; ``let``; ``func``]

type Literal = 
| Int of int
| Float of float
| Bool of bool
| String of string
| Value of string