open Parser
open Compiler
open TypedAST

let example = """
struct HelloAgain {
    hello : Hello
}

struct Hello { 
    hello : i32
}

func Hello (hello : i32, goodbye : f64) { 
    let x = 0
    let y = 1
    let bar = "hi"
    if true { 1 } 
    else { 
        let z = 2
        2
    }
}

func HelloAgain (hello : i32, goodbye : f64)  
    if true { 1 } 
    else { 
        let z = 2
        2
    }
"""

[<EntryPoint>]
let main argv = 
    let result = 
        rop {
            let! parseResult = parse example
            printfn "%A" parseResult
            let! functions, save = compile "test" parseResult
            let typeCheckErrors = functions |> List.map snd |> List.map checkExpr
            save()
            return ()
        } 
    printf "%s" "done!"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
