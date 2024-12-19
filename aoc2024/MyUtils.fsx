open System

let enumerate seq =
    seq |> Seq.mapi (fun i x -> (i, x))

// saving keystrokes:
let map fn = Seq.map fn
let el n (list:list<'a>) = list.[n]
let sort seq = Seq.sort seq
let len seq = Seq.length seq
let where seq = Seq.where seq

let str2chars (str:string) =
    str.ToCharArray() |> Array.toList

let toLines (str:string) =
    str.Split('\n') |> Seq.where ((<>) "")

let reMatches pattern str =
    let regex = System.Text.RegularExpressions.Regex(pattern)
    [ for matchObj in regex.Matches(str) -> matchObj.Value ]

let numbersInLine str =
    reMatches "\d+" str |> List.map int64

let linesAsNumbers lines =
    lines |> map numbersInLine
