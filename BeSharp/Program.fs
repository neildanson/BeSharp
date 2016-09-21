open Parser
open Compiler

[<EntryPoint>]
let main argv = 
    let example = """
struct Hello { 
    hello : i32
}

func Hello (hello : i32, goodbye : f64) -> f32 { 
    let x : i32 = 0;
    let y : i32 = 1;
    if (true) {
    } else {
    }
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
