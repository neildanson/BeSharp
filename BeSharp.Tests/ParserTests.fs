module Tests

open Xunit
open AST
open Parser

[<Theory>]
[<InlineData("struct Hello { hello : i32, goodbye : f64}")>]
[<InlineData("struct Hello {hello: i32, goodbye : f64}")>]
[<InlineData("struct Hello {hello : i32, goodbye : f64}")>]
[<InlineData("struct Hello { hello :i32, goodbye : f64}")>]
[<InlineData("struct Hello { hello : i32 , goodbye : f64}")>]
[<InlineData("struct Hello { hello : i32 ,goodbye : f64}")>]
[<InlineData("struct Hello { hello : i32 ,goodbye : f64 }")>]
[<InlineData(" struct Hello { hello : i32 ,goodbye : f64}")>] //TODO Handle leadint spaces
let ``struct parses`` example = 
    let parseResult = parse example
    let expected = [Struct("Hello", ["hello", "i32"; "goodbye", "f64"])]

    match parseResult with
    | ParseSuccess ast -> 
        Assert.Equal<File list>(expected, ast)
    | ParseFail message -> 
        Assert.False(true, message)


[<Theory>]
[<InlineData("func Hello (hello : i32, goodbye : f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello: i32, goodbye : f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32, goodbye : f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32 , goodbye : f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32,goodbye : f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32,goodbye: f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32,goodbye:f64) -> f32{ foo }")>]
[<InlineData("func Hello (hello:i32,goodbye:f64)-> f32{ foo }")>]
[<InlineData("func Hello (hello:i32,goodbye:f64)->f32{ foo }")>]
let ``func parses`` example = 
    let parseResult = parse example
    let expected = [Func("Hello", ["hello", "i32"; "goodbye", "f64"], "f32", Let("x", "i32"))]

    match parseResult with
    | ParseSuccess ast -> 
        Assert.Equal<File list>(expected, ast)
    | ParseFail message ->
        Assert.False(true, message)