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

// argh it's too late to understand active patterns. this seems fine
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

let input = File.ReadAllText("input.txt")
let result = part1 input
printfn "%d" result
