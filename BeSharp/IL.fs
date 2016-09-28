module IL

open TypedAST
open System.Reflection.Emit

let compile (methodBuilder:MethodBuilder) ast =
    let il = methodBuilder.GetILGenerator()
    let rec eval a =
        match a with
        | TLiteral(Int i) -> il.Emit(OpCodes.Ldc_I4, i)
        | TLiteral(Float f) -> il.Emit(OpCodes.Ldc_R8, f)
        | TLiteral(String s) -> il.Emit(OpCodes.Ldstr, s)
        | TLiteral(Bool(true)) -> il.Emit(OpCodes.Ldc_I4_1)
        | TLiteral(Bool(false)) -> il.Emit(OpCodes.Ldc_I4_0)
        | TBlock(exprs) -> exprs |> List.iter eval
        | TIf(cond, trueExpr, falseExpr) ->
            let falseCond = il.DefineLabel()
            let endLabel = il.DefineLabel()
            eval cond
            il.Emit(OpCodes.Brfalse, falseCond)
            eval trueExpr
            il.Emit(OpCodes.Br, endLabel)
            il.MarkLabel falseCond
            eval falseExpr
            il.MarkLabel endLabel
        | _ -> failwith "Not yet implemented"

    eval ast
    il.Emit OpCodes.Ret
