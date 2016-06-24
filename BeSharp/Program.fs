open Parser
open Compiler

[<EntryPoint>]
let main argv = 
    let example = """
struct Hello { 
    hello : i32, 
    goodbye : f64 
}

struct Goodbye {
    see_ya : Hello
}

struct Hello3 { 
    foo : i32, 
    bar : f64,
    bazz : f64 
}

func Hello (hello : i32, goodbye : f64) -> f32 { 
    foo 
}
    """
    let parseResult = parse example
    match parseResult with
    | ParseSuccess ast -> 
        compile "test" ast
        printf "%A\n\n" ast
        printf "%s" "done!"
    | ParseFail message -> printf "%s" message

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
