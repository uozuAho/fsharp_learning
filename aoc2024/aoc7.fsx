(*
https://adventofcode.com/2024/day/7

Just trying to hack together a 'quick' solution...
*)


let parseLine (line:string) =
    let result = int64 (line.Split(':').[0])
    let valStrs = line.Split(':').[1].Split()
    let vals = valStrs |> Array.where ((<>) "") |> Array.map int64 |> Array.toList
    result, vals

let genOperands numVals opBits =
    [0..numVals - 2]
    |> List.map (fun x ->
        if (opBits &&& (1 <<< x) = (1 <<< x)) then ( * ) else (+))

let rec doCalc vals operands result =
    match vals, operands with
    | [], [] -> result
    | v::vRest, o::oRest ->
        let newResult = o v result
        doCalc vRest oRest newResult
    | _, _ -> failwith $"val/op mismatch: {vals}, {operands}"

let op2str op =
    let result = op 3 3
    match result with
    | 6 -> '+'
    | 9 -> '*'
    | _ -> failwith "unknown op"

let rec check vals result counter =
    let numVals = List.length vals
    if (1 <<< (numVals-1)) <= counter then None
    else
    let operands = genOperands (List.length vals) counter
    let calcResult = doCalc vals[1..] operands vals.[0]
    // printfn $"  ops {operands |> List.map op2str} -> {calcResult}"
    if calcResult = result then
        Some(operands)
    else
        check vals result (counter + 1)

let rec sumGoodLines lines acc =
    match lines with
    | [] -> acc
    | line::rest ->
        let result, vals = parseLine line
        match check vals result 0 with
        | None ->
            sumGoodLines rest acc
        | Some(_) ->
            sumGoodLines rest (acc + result)

let input = "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"

// let lines = input.Split('\n') |> Array.where ((<>) "") |> Array.toList
let lines = System.IO.File.ReadAllLines "input.txt" |> Array.toList

// for line in lines do
//     printfn $"{line}"
//     let result, vals = parseLine line
//     let r = check vals result 0
//     match r with
//     | Some(operands) ->
//         printfn "yay"
//     | None ->
//         printfn "nope"

// printfn $"{sumGoodLines lines 0}"

(*
part 2: new op 'concat' || joins two nums, eg 12 || 34 = 1234
otherwise same as part 1
*)

type Operator = Add | Mult | Concat
#load "ScriptTest.fsx"
open ScriptTest
let tests = Tests()

let nextOpSet ops =
    let nextOp op =
        match op with
        | Add -> (Mult, false)
        | Mult -> (Concat, false)
        | Concat -> (Add, true)
    let rec nextOps ops carry =
        if not carry then ops
        else
        match ops with
        | [] -> ops
        | o::rest ->
            let newop, newCarry = if carry then nextOp o else o, false
            newop::(nextOps rest newCarry)
    nextOps ops true

for current, expNext in [
    ([Add; Add], [Mult; Add])
    ([Concat; Add], [Add; Mult])
    ([Concat; Concat], [Add; Add])
] do
    tests.add $"nextOpSet {current}" (fun _ ->
        let next = nextOpSet current
        assertEq next expNext)

let allOps numOps =
    let first = List.init numOps (fun _ -> Add)
    let last = List.init numOps (fun _ -> Concat)
    first |> Seq.unfold (fun current ->
        let next = nextOpSet current
        // todo: how to fix?
        // returns none: if current = first then None else Some(current, next)
        // misses last: if next = first then None else Some(current, next))

let doOp op x y =
    match op with
    | Add -> x + y
    | Mult -> x * y
    | Concat -> int ((string x) + (string y))

tests.run
