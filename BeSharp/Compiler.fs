module Compiler

open System.Reflection
open System.Reflection.Emit



let compile assembly ast = 
    let assemblyName = AssemblyName assembly
    let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.RunAndSave)

    let moduleBuilder = assemblyBuilder.DefineDynamicModule(assembly)
    
    assemblyBuilder.Save(sprintf "%s.dll" assembly)