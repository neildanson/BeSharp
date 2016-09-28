module Compiler

open System
open System.Reflection
open System.Reflection.Emit
open AST
open TypedAST
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
    

let rec toTyped types expr = 
    let toTyped = toTyped types 
    match expr with
    | Literal(x) -> TLiteral(x)
    | If(cond, trueExpr, falseExpr) -> TIf(toTyped cond, toTyped trueExpr, toTyped falseExpr)
    | Let(name, expr) -> let texpr = toTyped expr in let type' = getTypeOfExpr texpr in TLet(name, type', texpr)
    | Block(exprs) -> TBlock(exprs |> List.map toTyped)
    | _ -> failwith "IDK"



let defineStruct (moduleBuilder:ModuleBuilder) name fields =
    let structType = moduleBuilder.DefineType(name, TypeAttributes.Public, typeof<System.ValueType>)
    structType, fun customTypes -> 
        let fieldBuilders = fields |> List.map(fun (name, typeName) -> structType.DefineField(name, (resolveType typeName customTypes).Value, FieldAttributes.Public ||| FieldAttributes.InitOnly))
        structType, fieldBuilders, fun () -> 
            let ctor = structType.DefineConstructor(MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.RTSpecialName ||| MethodAttributes.HideBySig, CallingConventions.HasThis, fields |> List.map snd |> List.choose (fun t -> resolveType t customTypes) |> List.toArray)
            
            let il = ctor.GetILGenerator()
            fieldBuilders |> List.iteri(fun i field -> 
                il.Emit(OpCodes.Ldarg, 0)
                il.Emit(OpCodes.Ldarg, i + 1)
                il.Emit(OpCodes.Stfld, field)
                il.Emit(OpCodes.Ret))

            structType

let defineFunction (ownerType:TypeBuilder) name ast types = 
    let typedAst = toTyped types ast
    let returnType = getTypeOfExpr typedAst
    let methodBuilder = ownerType.DefineMethod(name, MethodAttributes.Static ||| MethodAttributes.Public, returnType, [|(*TODO*)|])
    methodBuilder, typedAst

let defineStructs moduleBuilder ast =
    ast |> List.fold(fun structs a -> match a with | Struct(name, fields) -> (defineStruct moduleBuilder name fields) :: structs | _ -> structs) []

let defineFunctions ownerType ast types = 
    ast |> List.fold(fun functions a -> match a with | Func(name,parameters,ast) -> (defineFunction ownerType name ast types) :: functions | _ -> functions) []

let compile assembly ast = 
    try
        let filename = sprintf "%s.dll" assembly
        let assemblyName = AssemblyName assembly
        let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Save)

        let moduleBuilder = assemblyBuilder.DefineDynamicModule(assemblyName.Name, filename, true)

        let structDefs = defineStructs moduleBuilder ast

        let customTypes = structDefs |> List.map fst

        let types = structDefs |> List.map snd |> List.map(fun f -> f customTypes) |> List.map(fun (tb,fbs,create) -> TStruct(tb,fbs), create)
        let structs = types |> List.map snd |> List.map (fun f -> f())
        structs |> List.iter(fun t -> t.CreateType() |> ignore)

        let functionClass = moduleBuilder.DefineType("Functions")
        let functions = defineFunctions functionClass ast structs

        functionClass.CreateType() |> ignore

        assemblyBuilder.Save(filename)
        Success()
    with
    | e -> Failure (e.Message)
