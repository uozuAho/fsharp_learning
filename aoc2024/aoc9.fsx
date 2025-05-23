(*
solve: dotnet fsi this_file
repl:  dotnet fsi --define:DEBUG --use:this_file

Remember to use asserts!
*)

// quiet the REPL a bit. not perfect
// 1854
// 12345 1 block file, 2 blocks free space, 3 block file, 4 blocks free space...
// 0..111....2222 <--- repeated file ids and empty blocks for file above
// compact files:
// 02.111....222.
// then checksum = sum(for block in files block * idx(block))
//
// 2333133121414131402
// 00...111...
// 0123456789   <-- pos
//
// your mission: defrag then checksum
//
// assumptions: you can't represent more than 9 files as a string, but there
// can be more than 9 files
fsi.ShowDeclarationValues <- false

#load "MyUtils.fsx"
#load "Assert.fsx"
#load "CharGrid.fsx"
open MyUtils
open Assert

let sampleStr = "
2333133121414131402
"

// note this only works for up to 9 files
// let diskmap2blocks (diskmap:string) =
//     str2chars diskmap
//     let rec doit id rem acc =
//         match rem with
//         | [] | [_] -> acc
//         | num::free::rest ->
//             let file = Array.create num (char id)
//             let freespace = Array.create free '.'
//             let newAcc = Array.concat [acc; file, freespace]

let dmap2arr (diskmap:string) =
    let chars = str2chars diskmap
    let rec doit fileId rem acc =
        match rem with
        | [] -> acc
        | x::[] ->
            let file = Array.create (int (string x)) fileId
            Array.concat [acc; file]
        | x::y::rest ->
            let file = Array.create (int (string x)) fileId
            let space = Array.create (int (string y)) -1
            let newAcc = Array.concat [acc; file; space]
            doit (fileId + 1) rest newAcc
    doit 0 chars [||]

// note this only works for up to 9 files
let arr2str arr =
    arr
    |> map (fun i -> if i = -1 then '.' else char (string i))
    |> chars2str

// functional's really going out the window here. I couldn't think of an
// elegant way to do it without mutation
let compact arr =
    let mutable left = 0
    let mutable right = Array.length arr - 1
    while left <= right do
        while arr[left] <> -1 do left <- left + 1
        while arr[right] = -1 do right <- right - 1
        if left < right then
            Array.set arr left arr[right]
            Array.set arr right -1
    arr

// let compactNoFrag arr =
//     let mutable rightR = Array.length arr - 1
//     let mutable rightL = Array.length arr - 1
//     while arr[rightR] = -1 do rightR <- rightR - 1
//     let fileId = arr[rightR]
//     while fileId > 0 && rightR > 0 do
//         let mutable left = 0
//     arr

let checksum arr =
    arr
    |> enumerate
    |> map (fun x -> if snd x = -1 then int64 0 else int64 (fst x * snd x))
    |> Seq.sum

let solve1 (lines:seq<string>) =
    dmap2arr (Seq.head lines) |> compact |> checksum

let solve2 lines =
    0

// todo: this needs session token to work
// manually save to input.txt for now
// let realInput = aocFetchInput 2024 7
let input = System.IO.File.ReadAllLines "input.txt"
let sample = str2Lines sampleStr

// let lineNumbers = sample |> linesAsNumbers
// let grid = CharGrid.fromLines sample

printfn "Part 1"
printfn "Sample:"
printfn $"{solve1 sample}"
printfn ""
printfn "Real input:"
printfn $"{solve1 input}"
printfn ""
printfn "Part 2"
printfn $"{solve2 sample}"
printfn ""
printfn "Real input:"
printfn $"{solve2 input}"
printfn ""
