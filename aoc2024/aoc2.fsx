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

type Trend = Up | Down | Flat | Unknown

let trend x y =
    if x - y > 0 then Down else
        if x - y = 0 then Flat else Up

// attempt 2. To fix:
// - make signature report -> bool
// - don't overlap checks (eg check a b c then b c d checks b c twice)
// - attempt 1 is wrong, passes 5 reports instead of 4
let isSafePart2_2 report =
    let isSafe x y prevTrend =
        (prevTrend = Unknown || trend x y = prevTrend)
        && withinBounds x y

    let rec check rem prevTrend canRemove =
        printfn "check %A, %A, %b" rem prevTrend canRemove
        match rem with
        | [] | [_] -> true
        | x::y::[] ->
            let prevTrend = if prevTrend <> Unknown then prevTrend else trend x y
            let safe = isSafe x y prevTrend
            printfn "at end. safe: %b" safe
            canRemove || safe
        | x::y::rest ->
            let prevTrend = if prevTrend <> Unknown then prevTrend else trend x y
            let safeHere = isSafe x y prevTrend
            match safeHere, canRemove with
            | false, false ->
                printfn "not safe & can't remove"
                false
            | false, true ->
                printfn "not safe, removing"
                check ([x] @ rest) prevTrend false
                || check ([y] @ rest) prevTrend false
            | true, _ ->
                check ([y] @ rest) prevTrend canRemove

    check report Unknown true

// 3rd attempt. Brute force. Shoulda started with this...
let genSubReports report =
    let len = List.length report
    [0..len-1] |> Seq.map (fun i -> List.removeAt i report)

let isSafePart2_3 report =
    if isSafe report then true
    else
        let safeReport = genSubReports report |> Seq.tryFind isSafe
        match safeReport with
        | Some(_) -> true
        | None -> false


let reports =
    File.ReadLines("input.txt")
    |> Seq.map parseReport
    |> Seq.toList

let printAllChecks check reports =
    reports |> Seq.iter (fun report ->
        printfn "%b: %A" (check report) report
    )

let count reports check =
    reports
    |> Seq.map check
    |> Seq.map (fun b -> if b then 1 else 0)
    |> Seq.sum

// printAllChecks isSafe reports
// printAllChecks isSafePart2_2 reports
// printAllChecks isSafePart2_3 reports
// printfn "num safe 1: %d" numSafePart1
// printfn "num safe 2: %d" numSafePart2
printfn "num safe 2: %d" (count reports isSafePart2_3)
