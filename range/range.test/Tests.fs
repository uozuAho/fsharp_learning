module Tests

open Xunit
open range.MyRange

[<Fact>]
let ``[2,6) contains {2,4}`` () =
    let range = Range(Closed 2, Open 6)
    Assert.True(contains range 2)
    Assert.True(contains range 4)

[<Fact>]
let ``[2,6) doesn’t contain {-1,1,6,10}`` () =
    let range = Range(Closed 2, Open 6)
    Assert.False(contains range -1)
    Assert.False(contains range 1)
    Assert.False(contains range 6)
    Assert.False(contains range 10)
