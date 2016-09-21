module AST

type Name = string
type TypeName = string
type MethodName = string

type Literal = 
| Int of int
| Float of float
| Bool of bool

type Expr =
| Literal of Literal
| If of Expr * Expr * Expr
| MethodCall of MethodName * (Expr list)
| Block of Body list

and Statement = 
| Let of Name * TypeName * Expr

and Body = 
| Expr of Expr
| Statement of Statement


type File = 
| Struct of Name * (Name * TypeName) list
| Func of Name * (Name * TypeName) list * TypeName * Body list