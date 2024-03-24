module Tests

open Xunit
open range.MyRange

[<Fact>]
let ``My test`` () =
    Assert.True(true)

let ``[2,6) contains {2,4}`` () =
    let range = Range(Closed 2, Open 6)
    Assert.True(contains range 2)
    Assert.True(contains range 4)
