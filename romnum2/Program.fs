open System

let toRoman x =
    (String.replicate x "I")
        .Replace("IIIII", "V")
        .Replace("IIII", "IV")


// ---------------------------------------------------------------------------
// tests
open Xunit
open FsCheck
open FsCheck.Xunit

let countChars char = Seq.filter ((=) char) >> Seq.length

[<Fact>] let ``throws on negative`` () =
    Assert.Throws<ArgumentException> (fun () -> toRoman -1 |> ignore)

[<Property>] let ``has max 3 IIIs`` () =
    let n = Gen.elements [1..4999] |> Arb.fromGen
    Prop.forAll n (fun i -> countChars 'I' (toRoman i) <= 3)