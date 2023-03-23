module Tests

open Xunit
open bowl.Bowling

[<Fact>]
let ``all gutters = 0`` () =
    let frames = [ for i in 1..10 -> Normal(0, 0) ]
    Assert.Equal(0, scoreGame frames)

[<Fact>]
let ``all 3s = 60`` () =
    let frames = [ for i in 1..10 -> Normal(3, 3) ]
    Assert.Equal(60, scoreGame frames)

[<Fact>]
let ``all spares with first ball 4 = 140`` () =
    let frames = [ for i in 1..10 -> Spare(4) ]
    Assert.Equal(140, scoreGame frames)
    
[<Fact>]
let ``strike followed by two 0s = 10`` () =
    let frames = [ Strike ] @ [ for i in 1..9 -> Normal(0, 0) ]
    Assert.Equal(10, scoreGame frames)
    
[<Fact>]
let ``strike followed by two 1s scores a total of 14`` () =
    let frames = [ Strike ] @ [Normal(1,1)] @ [ for i in 1..8 -> Normal(0, 0) ]
    Assert.Equal(14, scoreGame frames)
    
[<Fact(Skip="asdf")>]
let ``nine strikes followed by two gutters = 240`` () =
    let frames = [ for i in 1..9 -> Strike ] @ [ Normal(0, 0) ]
    Assert.Equal(240, scoreGame frames)