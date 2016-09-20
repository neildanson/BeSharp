module AST

type Name = string
type TypeName = string
type MethodName = string

type Expr = 
| If of Expr * Expr * Expr
| MethodCall of MethodName * (Expr list)

type AST = 
| Let of Name * TypeName //* Expr
| InstanceMethodCall of Name * MethodName


type File = 
| Struct of Name * (Name * TypeName) list
| Func of Name * (Name * TypeName) list * TypeName * AST