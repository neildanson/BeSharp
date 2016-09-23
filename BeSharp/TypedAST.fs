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
