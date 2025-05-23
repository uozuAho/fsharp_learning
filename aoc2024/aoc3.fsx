open System.Text.RegularExpressions
open System.IO
(*
https://adventofcode.com/2024/day/3

part 1:
Find all mul(x,y) in a string, and sum up all the multiplications.

part 2:
Same as above, but don't include any muls between a don't() and do(). Include
muls at the start until you encounter the first don't()
*)

// argh it's too late at night to understand active patterns. this seems fine:
let allMatches input pattern =
    let regex = Regex(pattern)
    [ for matchObj in regex.Matches(input) -> matchObj.Value ]

let findMuls str =
    allMatches str "mul\(\d+,\d+\)"

let getMulVals mulStr =
    let regex = Regex("(\d+),(\d+)")
    let matchObj = regex.Match(mulStr)
    int matchObj.Groups.[1].Value, int matchObj.Groups.[2].Value

let part1 str =
    findMuls str
    |> List.map getMulVals
    |> List.map (fun (x, y) -> x * y)
    |> List.sum

// Doesn't handle nested donts. Not sure if this is the problem
let filterDonts (str:string) =
    let regex = Regex("don't\(\)(.*?)do\(\)")
    let ret = regex.Replace(str, "")
    match ret.IndexOf("don't()") with
    | -1 -> ret
    | x -> ret[0..x-1]

// this gets the wrong answer. too low, ie filtering out too much
let part2 str =
    filterDonts str
    |> part1

// part 2 attempt 2. idea: indexof + recursive, just keep track of the last
// dont/do
let filterDonts2 str =
    let nextDoDontIdx (str:string) =
        let nextDo = str.IndexOf("do()")
        let nextDont = str.IndexOf("don't()")
        match nextDo, nextDont with
        | -1, -1 -> -1
        | -1, x -> x
        | x, -1 -> x
        | x, y  -> min x y

    let nextChunk str =
        let idx = nextDoDontIdx str
        match idx with
        | -1 -> str, ""
        | 0 ->
            if str.StartsWith("do()") then "do()", str[4..]
            else "don't()", str[7..]
        | x -> str[..x-1], str[x..]

    let rec go rem enabled result =
        match rem with
        | "" -> result
        | _ ->
            match nextChunk rem with
            | "do()", nextRem -> go nextRem true result
            | "don't()", nextRem -> go nextRem false result
            | chunk, nextRem ->
                let newResult = if enabled then (result + chunk) else result
                go nextRem enabled newResult

    go str true ""

let part2_2 str =
    filterDonts2 str
    |> part1


let input = File.ReadAllText("input.txt")
let result = part1 input
printfn "part1: %d" result
printfn "part2: %d" (part2 input)
printfn "part2_2: %d" (part2_2 input)
