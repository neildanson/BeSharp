module TypedAST

open AST

type TExpr = 
| TLiteral of Literal
| TLet of Name * System.Type * TExpr
| TIf of TExpr * TExpr * TExpr
| TBlock of TExpr list

type TFile = 
| TStruct of Name * (Name * System.Type) list
| TFunc of Name * (Name * System.Type) list * TExpr

let rec getTypeOfExpr = function
| TLiteral(Int _) -> typeof<int>
| TLiteral(Float _) -> typeof<float>
| TLiteral(Bool _) -> typeof<bool>
| TIf(_,_,elseExpr) -> getTypeOfExpr elseExpr
| TLet(_,_,expr) -> getTypeOfExpr expr
| TBlock(exprs) -> getTypeOfExpr (exprs |> List.last)

type TypeCheckResult = 
| TypeCheckSuccess 
| TypeCheckFail of string 

let check f error= if f() then TypeCheckSuccess else TypeCheckFail error

let checkIf expr trueExpr falseExpr =  
    [ check (fun () -> getTypeOfExpr expr = typeof<bool>) "If Condition must equate to boolean" 
      check (fun () -> (getTypeOfExpr trueExpr = getTypeOfExpr falseExpr)) "If Branches must have same return type" ]
    
let checkBlock exprs = 
    [ check (fun () -> exprs |> List.exists (fun _ -> true)) "Block must contain at least 1 expression" ]

let rec checkExpr = function 
| TIf (cond,trueExpr,falseExpr) -> 
    checkExpr cond @ checkExpr trueExpr @ checkExpr falseExpr @ checkIf cond trueExpr falseExpr
| TBlock exprs -> checkBlock exprs
| _ -> []


