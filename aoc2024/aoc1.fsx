(*
https://adventofcode.com/2024/day/1

Part 1:
- input = lines of pairs of numbers
- find the total distance between the sorted number
  pairs, ie sum(abs(a - b)) for (a, b) in lines

Part 2:
- same input
- find sum(x * num_appearances_in_right_list(x))

Usage:
xclip -o | dotnet fsi aoc1.fsx
dotnet fsi --use:aoc1.fsx
*)

#load "MyUtils.fsx"
open System
open MyUtils

let line2pair (line:string) =
    let ints =
        line.Split()
        |> Seq.where (fun x -> x <> "")
        |> Seq.map int
        |> Seq.toList
    match ints with
    | [a; b] -> Some(a, b)
    | _ -> None

let parsePairs lines =
    lines
    |> Seq.where (fun x -> not (String.IsNullOrEmpty x))
    |> Seq.map line2pair
    |> Seq.choose id

let stdin2pairs stdin =
    stdin |> parsePairs |> Seq.toList

let string2pairs (s:string) =
    s.Split('\n') |> parsePairs |> Seq.toList

let pairs2cols pairs =
    (pairs |> Seq.map fst,
    pairs |> Seq.map snd)

let totalDistance pairs =
    let left, right = pairs2cols pairs
    let left = left |> Seq.sort
    let right = right |> Seq.sort

    Seq.zip left right
    |> Seq.map (fun x -> fst x - snd x)
    |> Seq.map abs
    |> Seq.sum

let count x list =
    list
    |> Seq.map (fun y -> if y = x then 1 else 0)
    |> Seq.sum

let similarityScore pairs = 3

let solve pairs =
    printfn "distance:   %d" (totalDistance pairs)
    printfn "similarity: %d" (similarityScore pairs)

let solveIfStdin =
    match isStdInFromPipe with
    | true ->
        printfn "FROM STDIN:"
        let pairs = readStdin |> parsePairs
        solve pairs
    | _ -> ()

solveIfStdin
