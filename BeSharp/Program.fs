open Parser
open Compiler

[<EntryPoint>]
let main argv = 
    let parseResult = parse "struct Hello { hello : i32, goodbye : f64}"
    match parseResult with
    | ParseSuccess ast -> 
        compile "test" ast
        printf "%A\n\n" ast
        printf "%s" "done!"
    | ParseFail message -> printf "%s" message

    System.Console.ReadLine() |> ignore
    0 // return an integer exit code
