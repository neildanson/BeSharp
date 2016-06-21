module Tests

open Xunit
open Parser

[<Theory>]
[<InlineData("struct Hello { hello : i32, goodbye : f64}")>]
[<InlineData("struct Hello {hello: i32, goodbye : f64}")>]
[<InlineData("struct Hello {hello : i32, goodbye : f64}")>]
[<InlineData("struct Hello { hello :i32, goodbye : f64}")>]
[<InlineData("struct Hello { hello : i32 , goodbye : f64}")>]
[<InlineData("struct Hello { hello : i32 ,goodbye : f64}")>]
let ``struct parses`` example = 
    let parseResult = parse example
    let expected = Struct("Hello", ["hello", "i32"; "goodbye", "f64"])

    match parseResult with
    | ParseSuccess ast -> 
        Assert.Equal(expected, ast)
    | ParseFail message -> 
        Assert.False true