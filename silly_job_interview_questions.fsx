// Copy of https://chrispenner.ca/posts/interview
// run with dotnet fsi <this file>

// palindrome
let reverse = Seq.rev >> Seq.toArray >> System.String

let is_palindrome x = x = reverse x

printfn "-----------------------------"
printfn "palindromes"
printfn $"""racecar {is_palindrome "racecar"}"""


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

// todo: add to cheatsheet
// list append ::
// list concat @
// list expression

// todo: add some tests for combinations

// combinations
let rec combinations n a =
    match n, a with
    | 0, _ -> [[]]
    | n, x::rest ->
        let subs = [for sub in combinations (n-1) rest -> x::sub]
        let adsf = combinations n rest
        subs @ adsf
    | _, [] -> []


printfn "-----------------------------"
printfn "combinations"
printfn $"{combinations 1 [1;2;3]}"
