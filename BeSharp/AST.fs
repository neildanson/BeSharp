module AST

type Name = string
type TypeName = string
type MethodName = string

type Literal = 
| Int of int
| Float of float
| Bool of bool
| Value of string

type Expr =
| Literal of Literal
| Let of Name * TypeName * Expr
| If of Expr * Expr * Expr
| Block of Expr list

type File = 
| Struct of Name * (Name * TypeName) list
| Func of Name * (Name * TypeName) list * Expr

