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

[<Fact>]
let ``create from string`` () =
    let range = Range.tryCreate("[1, 2]")
    let expected = Range.tryCreate(Closed 1, Closed 2)
    match range, expected with
    | Some r, Some e -> Assert.Equal(r, e)
    | _ -> Assert.Fail("Invalid range")

[<Property>]
let ``same ranges are equal`` () =
    let n = validRangePairs
    Prop.forAll n (fun (a, b) ->
        let start =
            if a = b then Gen.constant (Closed a)
            else Gen.oneof [ gen { return Open a }; gen { return Closed a} ]
            |> Arb.fromGen
        let end_ =
            if a = b then Gen.constant (Closed b)
            else Gen.oneof [ gen { return Open b }; gen { return Closed b} ]
            |> Arb.fromGen
        Prop.forAll start (fun start ->
            Prop.forAll end_ (fun end_ ->
                let range1 = Range.tryCreate(start, end_)
                let range2 = Range.tryCreate(start, end_)
                match range1, range2 with
                | Some r1, Some r2 -> Assert.Equal(r1, r2)
                | _ -> Assert.Fail("Invalid range")
            )
        )
    )

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

[<Fact>]
let ``[1, 2] does not overlap [3, 4]`` () =
    let range1 = Range.tryCreate(Closed 1, Closed 2)
    let range2 = Range.tryCreate(Closed 3, Closed 4)
    match range1, range2 with
    | Some r1, Some r2 -> Assert.False(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[2, 3) does not overlap [3, 4]`` () =
    let range1 = Range.tryCreate("[2, 3)")
    let range2 = Range.tryCreate("[3, 4]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.False(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[2, 3] does not overlap (3, 4]`` () =
    let range1 = Range.tryCreate("[2, 3]")
    let range2 = Range.tryCreate("(3, 4]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.False(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[2, 3] overlaps [3, 4]`` () =
    let range1 = Range.tryCreate("[2, 3]")
    let range2 = Range.tryCreate("[3, 4]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[2, 4] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[2, 4]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[3, 4] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[3, 4]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``(3, 4] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("(3, 4]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[4, 5] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[4, 5]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[9, 10) overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[9, 10)")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[9, 10] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[9, 10]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[10, 11] overlaps [3, 10]`` () =
    let range1 = Range.tryCreate("[10, 11]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.True(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``(10, 11] does not overlap [3, 10]`` () =
    let range1 = Range.tryCreate("(10, 11]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.False(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")

[<Fact>]
let ``[11, 12] does not overlap [3, 10]`` () =
    let range1 = Range.tryCreate("[11, 12]")
    let range2 = Range.tryCreate("[3, 10]")
    match range1, range2 with
    | Some r1, Some r2 -> Assert.False(overlaps (r1, r2))
    | _ -> Assert.Fail("Invalid range")
