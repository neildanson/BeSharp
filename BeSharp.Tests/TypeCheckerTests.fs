module TypeCheckerTests

open Xunit
open Constants
open TypedAST

let hasErrors results = results |> List.exists (function Failure _ -> true | _ -> false)

[<Fact>]
let ``if cond must be boolean expression``() = 
    let expr = TIf(TLiteral(Bool(true)), TLiteral(Int 1), TLiteral(Int 1))
    let success =  checkExpr expr
    Assert.Equal(false, hasErrors success)

[<Fact>]
let ``if cond must be boolean expression 2``() = 
    let expr = TIf(TLiteral(Int(1)), TLiteral(Int 1), TLiteral(Int 1))
    let result = checkExpr expr
    Assert.Equal(true, hasErrors result)

[<Fact>]
let ``if branches must be of same type``() = 
    let expr = TIf(TLiteral(Bool(false)), TLiteral(Int 1), TLiteral(Int 2))
    let result = checkExpr expr
    Assert.Equal(false, hasErrors result)

[<Fact>]
let ``if branches must be of same type 2``() = 
    let expr = TIf(TLiteral(Bool(false)), TLiteral(Int 1), TLiteral(Bool false))
    let result = checkExpr expr
    Assert.Equal(true, hasErrors result)

[<Fact>]
let ``block must contain at least 1 expression``() = 
    let expr = TBlock []
    let result = checkExpr expr
    Assert.Equal(true, hasErrors result)


[<Fact>]
let ``block must contain at least 1 expression 2``() = 
    let expr = TBlock [TLiteral (Bool false)]
    let result = checkExpr expr
    Assert.Equal(false, hasErrors result)

[<Fact>]
let ``expression is a let binding``() = 
    let expr = TBlock [TLet("a", typeof<bool>, TLiteral(Bool(true)))
                       TRef("a")]
    Assert.True(true) //Assert type of expr = typeof<bool>

[<Fact>]
let ``last expression of a block cannot be a let binding``() = 
    let expr = TBlock [TLet("a", typeof<bool>, TLiteral(Bool(true)))]
    let result = checkExpr expr
    Assert.Equal(true, hasErrors result) 

[<Fact>]
let ``last expression of a block isnt a let binding``() = 
    let expr = TBlock [TLiteral(Int 1)]
    let result = checkExpr expr
    Assert.Equal(false, hasErrors result) 
