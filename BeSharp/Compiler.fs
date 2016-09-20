module Compiler

open System
open System.Reflection
open System.Reflection.Emit
open AST
open Parser

let (|BuiltIn|_|) (typeName, _) = 
    match typeName with
    | "i32" -> Some(typeof<System.Int32>)
    | "i64" -> Some(typeof<System.Int64>)
    | "f32" -> Some(typeof<System.Single>)
    | "f64" -> Some(typeof<System.Double>)
    | _ -> None

let (|Custom|_|) (typeName, customTypes : TypeBuilder list) = 
    customTypes |> List.tryFind (fun t -> t.Name = typeName) |> Option.map(fun t -> t :> Type)

let resolveType typeName customTypes = 
    match (typeName, customTypes) with
    | BuiltIn(type') -> Some type'
    | Custom(type') -> Some type'
    | _ -> None

let getBestEquality (type':Type) = 
    let specificEquals = type'.GetMethod("Equals", [|type'|])
    if specificEquals <> null then   
        specificEquals
    else 
        type'.GetMethod("Equals", [|typeof<obj>|])
    

let buildStruct (moduleBuilder:ModuleBuilder) name fields =
    let structType = moduleBuilder.DefineType(name, TypeAttributes.Public, typeof<System.ValueType>)
    structType, fun customTypes -> 
        let ctor = structType.DefineConstructor(MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig, CallingConventions.HasThis, fields |> List.map snd |> List.choose (fun t -> resolveType t customTypes) |> List.toArray)
        
        let fields = fields |> List.map(fun (name, typeName) -> structType.DefineField(name, (resolveType typeName customTypes).Value, FieldAttributes.Public ||| FieldAttributes.InitOnly))
        //let fieldsMap = fields |> List.map(fun f -> f.Name, f) |> Map.ofList

        let il = ctor.GetILGenerator()
        fields |> List.iteri(fun i field -> 
            il.Emit(OpCodes.Ldarg, 0)
            il.Emit(OpCodes.Ldarg, i + 1)
            il.Emit(OpCodes.Stfld, field)
            il.Emit(OpCodes.Ret))

        //TODO Lookup IEquatable and use?
        let createEquals() = 
            let equalsMethod = structType.DefineMethod("Equals", MethodAttributes.Public ||| MethodAttributes.Virtual, typeof<bool>, [|typeof<obj>|])
            let il = equalsMethod.GetILGenerator()
            let other = il.DeclareLocal(structType)
            other.SetLocalSymInfo ("other")
            let fail = il.DefineLabel()
            
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Unbox_Any, structType)
            il.Emit(OpCodes.Stloc, other)
            
            let temp = il.DeclareLocal typeof<bool>
            temp.SetLocalSymInfo ("temp")
            let length = List.length fields - 1
            fields |> List.iteri (fun i f -> 
                let fieldEquals = getBestEquality f.FieldType

                il.Emit(OpCodes.Ldloc, other) //load other
                il.Emit(OpCodes.Ldfld, f) //load other.field
                
                let xxx = il.DeclareLocal(f.FieldType)
                il.Emit(OpCodes.Ldloca, xxx)

                il.Emit(OpCodes.Ldarg_0) // load this
                il.Emit(OpCodes.Ldfld, f) //load this.field

                il.Emit(OpCodes.Call, fieldEquals) //call equals
                il.Emit(OpCodes.Stloc, temp)
                //if (i <> length) then
                //    il.Emit(OpCodes.Ldloc, temp)
                //    il.Emit(OpCodes.Brfalse, fail)
                il.Emit(OpCodes.Br, fail)
            )

            il.MarkLabel(fail)
            il.Emit(OpCodes.Ldloc, temp)
            il.Emit(OpCodes.Ret)

        createEquals()
        structType.CreateType() |> ignore

let compile assembly ast = 
    let filename = sprintf "%s.dll" assembly
    let assemblyName = AssemblyName assembly
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, filename, true)
    
    let structDefs = ast |> List.choose(fun f -> match f with
                                                 | Struct(name, fields) -> Some(name, fields)
                                                 | _ -> None )
    let structBuilders = structDefs |> List.map(fun (name, fields) -> buildStruct moduleBuilder name fields)
    let customTypes = structBuilders |> List.map fst
    structBuilders |> List.map snd |> List.iter(fun f -> f customTypes)
    
    assemblyBuilder.Save(filename)