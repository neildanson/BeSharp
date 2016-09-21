module Parser

open FParsec
open AST

let ws1 = spaces1
let str_ws1 s = pstring s .>> ws1
let str_ws s = pstring s .>> spaces

let pidentifierraw =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
let pidentifier =
    pidentifierraw 
    >>= fun s -> 
        //if reserved |> List.exists ((=) s) then fail "keyword" 
        //else 
        preturn s

type Lit = NumberLiteralOptions
let numberFormat = Lit.AllowMinusSign ||| Lit.AllowFraction ||| Lit.AllowExponent
let pnumber : Parser<Literal, unit> =
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Literal(int nl.String)
            else Literal(float nl.String)

let pidentifier_ws = pidentifier .>> spaces

//structs (e.g struct name { name : type, name : type }
let pfield = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pfields = sepBy pfield (str_ws ",") 
let pstructbody = between (str_ws "{") (str_ws "}") (pfields .>> spaces)
let pstruct = pipe3 (str_ws1 "struct") pidentifier_ws pstructbody (fun _ name body -> Struct(name, body))

let pliteral = pnumber |>> (fun v -> Number v)
let pexpr = pliteral .>> (str_ws ";")

//function body (e.g let x : i32 = 0)
let pletname = pipe2 (str_ws "let") pidentifier_ws (fun _ name -> name)
let plettype = pipe2 (str_ws ":") pidentifier_ws (fun _ typename -> typename)
let plet = pipe4 pletname plettype (str_ws "=") pexpr (fun name typename _ expr -> Statement(Let(name, typename, expr)))
let pbody = plet <|> plet
let pblock = many pbody 

//functions (e.g func name (param1 : type, param2 : type) -> type { BLOCK }
let pparameter = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pparameters = between (str_ws "(") (str_ws ")") (sepBy pparameter (str_ws ","))
let pfuncbody = between (str_ws "{") (str_ws "}") pblock //TODO
let pfuncname = pipe2  (str_ws1 "func") pidentifier_ws (fun _ name -> name)
let pfunc = pipe5 pfuncname pparameters (str_ws "->") (pidentifier_ws) pfuncbody (fun name parameters _ returnType body -> Func(name, parameters, returnType, body)) //TODO AST

let pfilebody = pstruct <|> pfunc

let pfile = many pfilebody

let pwsfile = spaces >>. pfile

type 'a Result = 
| ParseSuccess of 'a
| ParseFail of string

let parse str = 
    match run pwsfile str with
    | Success(result, _, _)   -> ParseSuccess result
    | Failure(errorMsg, _, _) -> ParseFail errorMsg