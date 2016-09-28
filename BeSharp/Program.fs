open Parser
open Compiler

let example = """
struct HelloAgain {
    hello : Hello
}

struct Hello { 
    hello : i32
}

func Hello (hello : i32, goodbye : f64) { 
    let x : i32 = 0
    let y : i32 = 1
    if true { 1 } 
    else { 
        let z : i32 = 2
        2
    }
}

func HelloAgain (hello : i32, goodbye : f64)  
    if true { 1 } 
    else { 
        let z : i32 = 2
        2
    }
"""

[<EntryPoint>]
let main argv = 
    let result = 
        rop {
            let! parseResult = parse example
            let! compiled = compile "test" parseResult
            return Success compiled
        } 
    printf "%s" "done!"

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
