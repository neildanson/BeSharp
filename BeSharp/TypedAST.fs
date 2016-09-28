module TypedAST

open AST
open System.Reflection.Emit

type TExpr = 
| TLiteral of Literal
| TLet of Name * System.Type * TExpr
| TIf of TExpr * TExpr * TExpr
| TBlock of TExpr list
| TRef of Name //Should this include type?

type TFile = 
| TStruct of TypeBuilder * (FieldBuilder) list
| TFunc of Name * (Name * System.Type) list * TExpr


let rec getTypeOfExpr = function
| TLiteral(Int _) -> typeof<int>
| TLiteral(Float _) -> typeof<float>
| TLiteral(Bool _) -> typeof<bool>
| TLiteral(String v) -> typeof<string>
| TIf(_,_,elseExpr) -> getTypeOfExpr elseExpr
| TLet(_,_,expr) -> getTypeOfExpr expr
| TBlock(exprs) -> getTypeOfExpr (exprs |> List.last)
| TLiteral(Value v) -> failwith "NotImplemented (yet)"
| TRef(name) -> failwith "NotImplemented (yet)"

type TypeCheckResult = 
| TypeCheckSuccess 
| TypeCheckFail of string 

let check f error= if f() then TypeCheckSuccess else TypeCheckFail error

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
| _ -> []


