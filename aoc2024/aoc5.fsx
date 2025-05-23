open System.IO
(*
https://adventofcode.com/2024/day/5

input example: 47|53 97|13 75,47,61,53,29 97,61,53,29,13

- the piped section contains ordering rules. 47 must be before 53, 97->13 etc.
- the next section is lines of page numbers
- sum the middle number of all correctly ordered lines

My assumptions
- rules cover all page numbers, ie for any pages a,b in a line, there is a rule
  that defines a & b's relationship
- all lines have odd numbers of pages
- rule sets are consistent, eg you won't get a|b, b|a or a|b, b|c, c|a

part 2: Sum the middle numbers of the incorrectly ordered lines, after sorting
the page numbers on that line according to the rules.
*)

type Rule = int * int

let testInput = "
47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
"

let parseRule (line:string) =
    let els = line.Split('|')
    int els[0], int els[1]

let parsePageNums (line:string) =
    line.Split(',') |> Array.map int |> Array.toList

let parseInput (str:string) =
    let lines = str.Split('\n')
    let rules =
        lines
        |> Array.where (fun x -> x.Contains('|'))
        |> Array.map parseRule
        |> Array.toList
    let pageSets =
        lines
        |> Array.where (fun x -> x.Contains(','))
        |> Array.map parsePageNums
        |> Array.toList
    rules, pageSets

let compare a b rules =
    if a = b then 0
    else
    let matchingRule =
        rules
        |> Seq.tryFind (fun (x, y) -> (a = x && b = y) || (a = y && b = x))
    match matchingRule with
    | None -> failwith $"No rule found for {a}, {b}"
    | Some(x, y) ->
        match a - x with
        | 0 -> -1   // x = a,  rule is a|b
        | _ -> 1    // x != a, rule is b|a

let rec obeysRules rules pageSet =
    match pageSet with
    | [] | [_] -> true
    | x::y::rest ->
        -1 = compare x y rules
        && obeysRules rules (y::rest)

let middleNum pageSet =
    let idx = List.length pageSet / 2
    pageSet.[idx]

// let rules, pageSets = parseInput testInput
let rules, pageSets = parseInput (File.ReadAllText "input.txt")

// for pset in pageSets do
//     let obeys = obeysRules pset rules
//     printfn $"{obeys}: {middleNum pset}: {pset}"

let part1sum rules pageSets =
    pageSets
    |> Seq.where (fun pset -> obeysRules rules pset)
    |> Seq.map middleNum
    |> Seq.sum

let part2sum rules pageSets =
    let comparer a b =
        compare a b rules

    pageSets
    // |> Seq.iter (fun pset ->
    //     printfn $"{pset}: {not (obeysRules rules pset)}")
    |> Seq.where (fun pset -> not (obeysRules rules pset))
    |> Seq.map (fun pset -> pset |> List.sortWith comparer)
    |> Seq.map middleNum
    |> Seq.sum

printfn $"{part1sum rules pageSets}"
printfn $"{part2sum rules pageSets}"
