module Compiler

open System.Reflection
open System.Reflection.Emit
open Parser

let getBuiltInType = function
| "i32" -> Some(typeof<System.Int32>)
| "i64" -> Some(typeof<System.Int64>)
| "f32" -> Some(typeof<System.Single>)
| "f64" -> Some(typeof<System.Double>)
| _ -> None



let buildStruct (moduleBuilder:ModuleBuilder) name fields =
    let structType = moduleBuilder.DefineType(name, TypeAttributes.Public, typeof<System.ValueType>)
    let ctor = structType.DefineConstructor(MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName, CallingConventions.Any, fields |> List.map snd |> List.choose getBuiltInType |> List.toArray)
    
    //TODO - make a Map
    let fields = fields |> List.map(fun (name, typeName) -> structType.DefineField(name, (getBuiltInType typeName).Value, FieldAttributes.Public ||| FieldAttributes.InitOnly))
    let il = ctor.GetILGenerator()
    //TODO - emit proper IL code
    il.Emit(OpCodes.Ldarga, 1)
    il.Emit(OpCodes.Stfld, fields.[0])
    il.Emit(OpCodes.Ldarga, 2)
    il.Emit(OpCodes.Stfld, fields.[1])
    structType.CreateType() |> ignore

let compile assembly ast = 
    let filename = sprintf "%s.dll" assembly
    let assemblyName = AssemblyName assembly
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, filename)
    
    ast |> List.choose(fun f -> match f with
                                | Struct(name, fields) -> Some(name, fields)
                                | _ -> None )
        |> List.iter(fun (name, fields) -> buildStruct moduleBuilder name fields)


    assemblyBuilder.Save(filename)