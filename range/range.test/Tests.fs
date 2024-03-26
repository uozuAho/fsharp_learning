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
        let range = Range.tryCreate(Closed a, Closed b)
        match range with
        | Some range -> contains range a && contains range b
        | None -> Assert.Fail("Invalid range"); false
    )

[<Property>]
let ``(a,b) does not contain a or b`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let newB = b + 1  // to ensure not degenerate range
        let range = Range.tryCreate(Open a, Open newB)
        match range with
        | Some range -> not (contains range a) && not (contains range newB)
        | None -> Assert.Fail("Invalid range"); false
    )

[<Property>]
let ``(a, b+2) contains a + 1`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let range = Range.tryCreate(Open a, Open (b + 2))
        match range with
        | Some range -> contains range (a + 1)
        | None -> Assert.Fail("Invalid range"); false
    )

[<Property>]
let ``(a, a) is invalid`` (a: int) =
    let invalidRange = Range.tryCreate(Open a, Open a)
    Assert.Equal(invalidRange, None)

[<Property>]
let ``[a, a) is invalid`` (a: int) =
    let invalidRange = Range.tryCreate(Closed a, Open a)
    Assert.Equal(invalidRange, None)

[<Property>]
let ``(a, a] is invalid`` (a: int) =
    let invalidRange = Range.tryCreate(Open a, Closed a)
    Assert.Equal(invalidRange, None)
