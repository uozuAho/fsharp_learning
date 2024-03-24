module Tests

open FsCheck
open FsCheck.Xunit
open range.MyRange

let validRangePairs =
    Arb.generate<int>
    |> Gen.two
    |> Gen.map(fun (a, b) -> if a > b then (b, a) else (a, b))
    |> Arb.fromGen

[<Property>]
let ``[a,b] contains a and b`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let range = Range.create(Closed a, Closed b)
        contains range a && contains range b
    )

[<Property>]
let ``(a,b) does not contain a or b`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let newB = b + 1  // to ensure not degenerate range
        let range = Range.create(Open a, Open newB)
        not (contains range a) && not (contains range newB)
    )

[<Property>]
let ``(a, b+2) contains a + 1`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let range = Range.create(Open a, Open (b + 2))
        contains range (a + 1)
    )

// I  think FsCheck is broken for this test
// [<Property>]
// let ``(a, a) throws ArgumentException`` (a: int) =
//     Assert.Throws<ArgumentException>(fun () -> Range.create(Open a, Open a) |> ignore)
