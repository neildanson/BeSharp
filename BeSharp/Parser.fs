module Parser

open FParsec
open AST

let ws = spaces
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
            if nl.IsInteger then Int(int nl.String)
            else Float(float nl.String)

let pidentifier_ws = pidentifier .>> spaces

//structs (e.g struct name { name : type, name : type }
let pfield = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pfields = sepBy pfield (str_ws ",") 
let pstructbody = between (str_ws "{") (str_ws "}") pfields
let pstruct = pipe3 (str_ws1 "struct") pidentifier_ws pstructbody (fun _ name body -> Struct(name, body))

let pexpr, pexprimpl = createParserForwardedToRef ()
//expression (e.g. if, literal, etc)
let pliteral = pnumber |>> Literal
let ptrue = str_ws "true" |>> fun _ -> Literal(Bool true)
let pfalse = str_ws "false" |>> fun _ -> Literal(Bool false)
let pvalue = pliteral <|> ptrue <|> pfalse 

//if expression (e.g. if true { true } else { false }
let pif = pipe5 (str_ws1 "if") pexpr ((str_ws "{") >>. pexpr .>> str_ws "}")  (str_ws1 "else") ((str_ws "{") >>. pexpr .>> str_ws "}") (fun _ cond trueExpr _ falseExpr -> If(cond, trueExpr, falseExpr))

//let binding (e.g let x : i32 = 0)
let plet = pipe3 (str_ws1 "let" >>. pidentifier_ws) (str_ws1 ":" >>. pidentifier_ws) (str_ws "=" >>. pexpr) (fun name typename expr -> Let(name, typename, expr))

//function body if/let/value/block
let pbody = attempt plet <|> pif <|> pvalue .>> spaces

let opp = OperatorPrecedenceParser<Expr,unit,unit>()
pexprimpl := opp.ExpressionParser
let term = pbody
opp.TermParser <- term

//functions (e.g func name (param1 : type, param2 : type) -> type { BLOCK }
let pparameter = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pparameters = between (str_ws "(") (str_ws ")") (sepBy pparameter (str_ws ","))
let pfunc = pipe4 ((str_ws1 "func") >>. pidentifier_ws) pparameters ((str_ws "->") >>. pidentifier_ws) (between (str_ws "{") (str_ws "}") (many pbody)) (fun name parameters returnType body -> Func(name, parameters, returnType, body)) //TODO AST

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