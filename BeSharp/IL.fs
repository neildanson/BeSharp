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
        | TBlock(exprs) -> exprs |> List.iter eval
        | _ -> failwith "Not yet implemented"

    eval ast
    il.Emit OpCodes.Ret
