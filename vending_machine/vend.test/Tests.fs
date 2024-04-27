module Tests

open Xunit
open vend.Say

[<Fact>]
let ``My test`` () =
    Assert.Equal("hello bert", hello "bert")
