// seqs
let enumerate seq = seq |> Seq.mapi (fun i x -> (i, x))
let map fn = Seq.map fn
let el n (list:list<'a>) = list.[n]
let sort seq = Seq.sort seq
let len seq = Seq.length seq
let where seq = Seq.where seq

// strings
let str2chars (str:string) = str.ToCharArray() |> Array.toList
let str2Lines (str:string) = str.Split('\n') |> Seq.where ((<>) "")

let reMatches pattern str =
    let regex = System.Text.RegularExpressions.Regex(pattern)
    [ for matchObj in regex.Matches(str) -> matchObj.Value ]

// aoc specific stuff
let numbersInLine str = reMatches "\d+" str |> List.map int64

let linesAsNumbers lines =
    lines |> map numbersInLine
