module IL

open TypedAST
open System.Reflection.Emit

let compile (methodBuilder:MethodBuilder) func =
    let il = methodBuilder.GetILGenerator()
    let rec eval ast (locals:Map<string, LocalBuilder>) =
        match ast with
        | TLiteral(Int i) -> il.Emit(OpCodes.Ldc_I4, i)
                             locals
        | TLiteral(Float f) -> il.Emit(OpCodes.Ldc_R8, f)
                               locals
        | TLiteral(String s) -> il.Emit(OpCodes.Ldstr, s)
                                locals
        | TLiteral(Bool(true)) -> il.Emit(OpCodes.Ldc_I4_1)
                                  locals
        | TLiteral(Bool(false)) -> il.Emit(OpCodes.Ldc_I4_0)
                                   locals
        | TBlock(exprs) -> 
            exprs |> List.fold (fun locals ast -> (eval ast locals)) locals
        | TIf(cond, trueExpr, falseExpr) ->
            let trueCond = il.DefineLabel()
            let endLabel = il.DefineLabel()
            let locals = eval cond locals
            il.Emit(OpCodes.Brtrue, trueCond) 
            let locals = eval falseExpr locals
            il.Emit(OpCodes.Br, endLabel)
            il.MarkLabel trueCond
            let locals = eval trueExpr locals
            il.MarkLabel endLabel
            locals
        | TLet(name, type', expr) -> 
            let local = il.DeclareLocal(type')
            local.SetLocalSymInfo name
            let locals = locals |> Map.add name local 
            let locals = eval expr locals
            il.Emit(OpCodes.Ldloc_0)
            locals
        | TRef(name, type') -> 
            //Might have to handle struct/class different
            il.Emit(OpCodes.Ldloc, locals |> Map.find name) 
            locals
        | _ -> failwith "Not yet implemented"

    match func with
    | TFunc(name, parameters, ast) -> ignore (eval ast Map.empty)
                                      il.Emit OpCodes.Ret
    | _ -> failwith "I dont think this can ever happen"
