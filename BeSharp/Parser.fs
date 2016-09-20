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

let pidentifier_ws = pidentifier .>> spaces

//structs (e.g struct name { name : type, name : type }
let pfield = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pfields = sepBy pfield (str_ws ",") 
let pstructbody = between (str_ws "{") (str_ws "}") (pfields .>> spaces)
let pstruct = pipe3 (str_ws1 "struct") pidentifier_ws pstructbody (fun _ name body -> Struct(name, body))

//functions (e.g func name (param1 : type, param2 : type) -> type { BODY }
let pparameter = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pparameters = between (str_ws "(") (str_ws ")") ((sepBy pparameter (str_ws ",")) .>> spaces)
let pfuncbody = between (str_ws "{") (str_ws "}") pidentifier_ws //TODO
let pfuncname = pipe2  (str_ws1 "func") pidentifier_ws (fun _ name -> name)
let pfunc = pipe5 pfuncname pparameters (str_ws "->") (pidentifier_ws) pfuncbody (fun name parameters _ returnType body -> Func(name, parameters, returnType, Let("x", "i32"))) //TODO AST

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