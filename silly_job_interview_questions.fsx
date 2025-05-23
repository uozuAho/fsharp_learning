// Copy of https://chrispenner.ca/posts/interview
// run with dotnet fsi <this file>

#r "nuget: Faqt"
open Faqt

// -----------------------------------------
// palindrome
let reverse = Seq.rev >> Seq.toArray >> System.String

let is_palindrome x = x = reverse x

printfn "-----------------------------"
printfn "palindromes"
printfn $"""racecar {is_palindrome "racecar"}"""


// -----------------------------------------
// fizzbuzz
let fizz n =
    match n % 3, n % 5 with
    | 0, 0 -> "fizzbuzz"
    | 0, _ -> "fizz"
    | _, 0 -> "buzz"
    | _ -> string n


printfn "-----------------------------"
printfn "fizzbuzz"
[1..10] |> List.iter (fizz >> printfn "%s")


// -----------------------------------------
// sum to n problem

let rec combinations n a =
    match n, a with
    | 0, _ -> [[]]
    | n, x::rest ->
        let subs = [for sub in combinations (n-1) rest -> x::sub]
        let adsf = combinations n rest
        subs @ adsf
    | _, [] -> []

(combinations 0 []).Should().Be [[]]
(combinations 3 []).Should().Be []
(combinations 3 [1]).Should().Be []
(combinations 3 [1;2;3]).Should().Be [[1;2;3]]
(combinations 1 [1;2;3]).Should().Be [[1];[2];[3]]

let sumNToTotal n total (list:list<int>) =
    let sumsToTotal sub = total = List.sum sub
    combinations n list |> List.filter sumsToTotal

printfn "-----------------------------"
printfn "sumNToTotal"
printfn $"{sumNToTotal 3 15 [2; 5; 3; 10; 4; 1; 0]}"

let sumAnyToTarget total list =
    [for n in [0..List.length list] -> sumNToTotal n total list]
        |> List.filter (List.isEmpty >> not)
        |> List.concat

printfn "sumAnyToTarget 15 [2; 5; 3; 10; 4; 1; 0]"
sumAnyToTarget 15 [2; 5; 3; 10; 4; 1; 0] |> List.iter (fun x -> printfn "  %A" x)
