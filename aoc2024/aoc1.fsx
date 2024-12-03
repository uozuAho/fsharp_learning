(*
https://adventofcode.com/2024/day/1

In short:
- input = lines of pairs of numbers
- find the total distance between the sorted number
  pairs, ie sum(abs(a - b)) for (a, b) in lines
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

let input2pairs =
    readStdin
    |> Seq.where (fun x -> not (String.IsNullOrEmpty x))
    |> Seq.map line2pair
    |> Seq.choose id

let pairs = input2pairs |> Seq.toList
let left = pairs |> Seq.map fst |> Seq.sort;
let right = pairs |> Seq.map snd |> Seq.sort;

let answer =
    Seq.zip left right
    |> Seq.map (fun x -> fst x - snd x)
    |> Seq.map abs
    |> Seq.sum

printfn "%d" answer
