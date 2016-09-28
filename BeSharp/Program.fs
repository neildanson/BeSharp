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

let test = """
func Hello0 () "Hi"
func Hello1 () 44
func Hello2 () { 45 }
func Hello3 () true
func Hello4 () false
func Hello5 () if true 1 else 2
func Hello6 () if true 1 else if false 2 else 3
func Hello7 () { let x = if true 1 else 2
                 46 }
"""

[<EntryPoint>]
let main argv = 
    let result = 
        rop {
            let! parseResult = parse test
            printfn "%A" parseResult
            let! functions, save = compile "test" parseResult
            let typeCheckErrors = functions |> List.map snd |> List.map checkExpr
            save()
            return ()
        } 
    printf "%s" "done!"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
