open System.IO
(*
https://adventofcode.com/2024/day/2

input: one "levels" report per line:

7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9

Report is safe if:
- levels are all increasing or all decreasing
- adjacent levels differ by 1 <= x <= 3
*)

type Report = list<int>
type Trend = Increasing | Decreasing | NoChange | Unknown

let parseReport (line:string): Report =
    line.Split(' ')
    |> Seq.where ((<>) "")
    |> Seq.map int
    |> Seq.toList

let str2reports (str:string) =
    str.Split('\n')
    |> Seq.map parseReport

let rec isMonotonic report =
    match report with
    | [] | [_] | [_;_] -> true
    | x::y::z::[] ->
        match x, y, z with
        | _ when x - y > 0 && y - z < 0 -> false
        | _ when x - y < 0 && y - z > 0 -> false
        | _ -> true
    | x::y::z::rest ->
        isMonotonic ([y; z] @ rest)

let withinBounds x y =
    let absdiff = abs (x - y)
    absdiff >= 1 && absdiff <= 3

let rec allWithinBounds report =
    match report with
    | x::y::[] -> withinBounds x y // TODO not quitting early when false found
    | x::y::rest -> allWithinBounds ([y] @ rest)
    | _ -> true

let isSafe report =
    isMonotonic report && allWithinBounds report

let reports =
    File.ReadLines("input.txt")
    |> Seq.map parseReport
    |> Seq.toList

// reports |> Seq.iter (fun report ->
//     printfn "%b: %A" (isSafe report) report
// )

let numSafe =
    reports
    |> Seq.map isSafe
    |> Seq.map (fun b -> if b then 1 else 0)
    |> Seq.sum

printfn "num safe: %d" numSafe
