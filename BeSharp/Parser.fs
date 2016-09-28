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
        if keywords |> List.exists ((=) s) then fail "keyword" 
        else preturn s

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
let pstruct = pipe3 (str_ws1 ``struct``) pidentifier_ws pstructbody (fun _ name body -> Struct(name, body))

let pexpr, pexprimpl = createParserForwardedToRef ()
//literal (e.g. if, 1, 1.0, true, false etc)
let pliteral = pnumber |>> Literal
let ptrue = str_ws "true" |>> fun _ -> Literal(Bool true)
let pfalse = str_ws "false" |>> fun _ -> Literal(Bool false)
let pstring = between (str_ws "\"") (str_ws "\"") pidentifier |>> (fun s -> Literal(String s))
let pvalue = pliteral <|> ptrue <|> pfalse <|> pstring

//let binding (e.g let x : i32 = 0)
let plet = pipe2 (str_ws1 "let" >>. pidentifier_ws) (str_ws "=" >>. pexpr) (fun name expr -> Let(name, expr))

//Block (e.g { expr1 expr2 }
let pblock = between (str_ws "{") (str_ws "}") (many pexpr) |>> Block

//if expression (e.g. if true trueExpr else falseExpr
let pif = pipe5 (str_ws1 ``if``) pexpr pexpr  (str_ws1 ``else``) pexpr (fun _ cond trueExpr _ falseExpr -> If(cond, trueExpr, falseExpr))
let pref = pidentifier_ws |>> Ref

//function body if/let/value/block
let pbody = attempt plet <|> pif <|> pblock <|> pvalue <|> pref .>> spaces

let opp = OperatorPrecedenceParser<Expr,unit,unit>()
pexprimpl := opp.ExpressionParser
let term = pbody
opp.TermParser <- term

//functions (e.g func name (param1 : type, param2 : type)  Expr
let pparameter = pipe3 pidentifier_ws (str_ws ":") pidentifier_ws (fun name _ typename -> name, typename)
let pparameters = between (str_ws "(") (str_ws ")") (sepBy pparameter (str_ws ","))
let pfunc = pipe3 ((str_ws1 ``func``) >>. pidentifier_ws) pparameters pexpr (fun name parameters body -> Func(name, parameters, body)) //TODO AST

let pfilebody = pstruct <|> pfunc

let pfile = many pfilebody

let pwsfile = spaces >>. pfile

let parse str = 
    match run pwsfile str with
    | Success(result, _, _)   -> ROP.Success result
    | Failure(errorMsg, _, _) -> ROP.Failure errorMsg