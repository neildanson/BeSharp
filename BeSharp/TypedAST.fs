module TypedAST

open System.Reflection.Emit

type TExpr = 
| TLiteral of Literal
| TLet of Name * System.Type * TExpr
| TIf of TExpr * TExpr * TExpr
| TBlock of TExpr list
| TRef of Name * System.Type option

type TFile = 
| TStruct of TypeBuilder * (FieldBuilder) list
| TFunc of Name * (Name * System.Type) list * TExpr

let rec getTypeOfExpr = function 
| TLiteral(Int _) -> typeof<int>
| TLiteral(Float _) -> typeof<float>
| TLiteral(Bool _) -> typeof<bool>
| TLiteral(String v) -> typeof<string>
| TIf(_,_,elseExpr) -> getTypeOfExpr elseExpr
| TLet(name,type',expr) -> type'
| TBlock(exprs) -> 
    getTypeOfExpr (exprs |> List.last) 
| TRef(name, type') -> match type' with Some type' -> type'  | None -> failwith (sprintf "Invalid type for %s" name)
| TLiteral(Value v) -> failwith "NotImplemented (yet)"

let check f error= if f() then Success () else Failure error

let checkIf expr trueExpr falseExpr =  
    [ check (fun () -> getTypeOfExpr expr = typeof<bool>) "If Condition must equate to boolean" 
      check (fun () -> (getTypeOfExpr trueExpr = getTypeOfExpr falseExpr)) "If Branches must have same return type" ]
    
let checkBlock exprs = 
    [ check (fun () -> exprs |> List.exists (fun _ -> true)) "Block must contain at least 1 expression" 
      check (fun () -> let last = exprs |> List.tryLast in match last with | Some(TLet _) -> false | _ -> true ) "Last expression of a block cannot be a let binding" ]

let rec checkExpr = function 
| TIf (cond,trueExpr,falseExpr) -> 
    checkExpr cond @ checkExpr trueExpr @ checkExpr falseExpr @ checkIf cond trueExpr falseExpr
| TBlock exprs -> checkBlock exprs
| TRef(name, type') -> [ yield match type' with | Some _ -> Success () | None -> Failure (sprintf "Invalid type for %s" name) ]
| _ -> []


let checkAst ast = 
    ast |> List.collect(fun a -> match a with | TFunc(name, parameters, expr) -> checkExpr expr | _ -> [])