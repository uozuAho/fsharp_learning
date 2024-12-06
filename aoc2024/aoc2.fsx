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

Part 2
Report is now safe if:
- safe as per previous rules
- removing any one level from the report makes it safe
*)

type Report = list<int>

let parseReport (line:string): Report =
    line.Split(' ')
    |> Seq.where ((<>) "")
    |> Seq.map int
    |> Seq.toList

let str2reports (str:string) =
    str.Split('\n')
    |> Seq.map parseReport

let ismono x y z =
    match x, y, z with
    | _ when x - y > 0 && y - z < 0 -> false
    | _ when x - y < 0 && y - z > 0 -> false
    | _ -> true

let rec isMonotonic report =
    match report with
    | x::y::z::[] -> ismono x y z
    | x::y::z::rest ->
        if not (ismono x y z) then false
        else isMonotonic ([y;z] @ rest)
    | _ -> true

let withinBounds x y =
    let absdiff = abs (x - y)
    absdiff >= 1 && absdiff <= 3

let rec allWithinBounds report =
    match report with
    | x::y::[] -> withinBounds x y
    | x::y::rest ->
        if not (withinBounds x y) then false
        else allWithinBounds ([y] @ rest)
    | _ -> true

let isSafe report =
    isMonotonic report && allWithinBounds report

let rec isSafePart2 report isOneRemoved =
    let isSliceSafe x y z =
        ismono x y z && withinBounds x y && withinBounds y z

    match report with
    | [] | [_] -> true
    | [x; y] -> if isOneRemoved then withinBounds x y else true
    | x::y::z::[] ->
        match (isSliceSafe x y z), isOneRemoved with
        | true, _ -> true
        | false, true -> false
        | false, false ->
            isSafePart2 [x; y] true
            || isSafePart2 [x; z] true
            || isSafePart2 [y; z] true
    | x::y::z::rest ->
        if (isSliceSafe x y z)
        then isSafePart2 ([y; z] @ rest) isOneRemoved
        else isSafePart2 ([x; y] @ rest) true
            || isSafePart2 ([x; z] @ rest) true
            || isSafePart2 ([y; z] @ rest) true

let reports =
    File.ReadLines("input.txt")
    |> Seq.map parseReport
    |> Seq.toList

let printAllChecks check reports =
    reports |> Seq.iter (fun report ->
        printfn "%b: %A" (check report) report
    )

let numSafePart1 =
    reports
    |> Seq.map isSafe
    |> Seq.map (fun b -> if b then 1 else 0)
    |> Seq.sum

let numSafePart2 =
    reports
    |> Seq.map (fun report -> isSafePart2 report false)
    |> Seq.map (fun b -> if b then 1 else 0)
    |> Seq.sum

// printAllChecks isSafe reports
printAllChecks (fun r -> isSafePart2 r false) reports
printfn "num safe 1: %d" numSafePart1
printfn "num safe 2: %d" numSafePart2
