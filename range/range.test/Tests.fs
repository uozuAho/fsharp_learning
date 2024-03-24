module Tests

open Xunit
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
        let range = Range.create(Open a, Open (b + 1))
        not (contains range a) && not (contains range b)
    )

[<Property>]
let ``(a, b+2) contains a + 1`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let range = Range.create(Open a, Open (b + 2))
        contains range (a + 1)
    )
