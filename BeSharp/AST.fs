module AST

type Expr =
| Literal of Literal
| Let of Name * TypeName * Expr
| If of Expr * Expr * Expr
| Block of Expr list
| Ref of string

type File = 
| Struct of Name * (Name * TypeName) list
| Func of Name * (Name * TypeName) list * Expr

