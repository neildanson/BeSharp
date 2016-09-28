module IL

open TypedAST
open System.Reflection.Emit

let compile (methodBuilder:MethodBuilder) ast =
    let il = methodBuilder.GetILGenerator()
    let rec eval ast locals =
        match ast with
        | TLiteral(Int i) -> il.Emit(OpCodes.Ldc_I4, i)
        | TLiteral(Float f) -> il.Emit(OpCodes.Ldc_R8, f)
        | TLiteral(String s) -> il.Emit(OpCodes.Ldstr, s)
        | TLiteral(Bool(true)) -> il.Emit(OpCodes.Ldc_I4_1)
        | TLiteral(Bool(false)) -> il.Emit(OpCodes.Ldc_I4_0)
        | TBlock(exprs) -> exprs |> List.iter (fun ast -> eval ast locals)
        | TIf(cond, trueExpr, falseExpr) ->
            let trueCond = il.DefineLabel()
            let endLabel = il.DefineLabel()
            eval cond locals
            il.Emit(OpCodes.Brtrue, trueCond) 
            eval falseExpr locals
            il.Emit(OpCodes.Br, endLabel)
            il.MarkLabel trueCond
            eval trueExpr locals
            il.MarkLabel endLabel
        | TLet(name, type', expr) -> 
            let local = il.DeclareLocal(type')
            local.SetLocalSymInfo name
            eval expr (local :: locals)
            il.Emit(OpCodes.Ldloc_0)
        | _ -> failwith "Not yet implemented"

    eval ast []
    il.Emit OpCodes.Ret
