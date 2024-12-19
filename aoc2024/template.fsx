(*
solve: dotnet fsi this_file
repl:  dotnet fsi --define:DEBUG --use:this_file

Remember to use asserts!
*)

// quiet the REPL a bit. not perfect
fsi.ShowDeclarationValues <- false

#load "MyUtils.fsx"
#load "Assert.fsx"
#load "CharGrid.fsx"
open MyUtils
open Assert

let sampleStr = "
3   4
4   3
2   5
1   3
3   9
3   3
"

let solve1 lines =
    // example usage of some of my utils. remove once you've done a few more puzzles
    let numberLines = linesAsNumbers lines
    let col1 = numberLines |> map (el 0)
    let col2 = numberLines |> map (el 1)
    wozAssert ((len col1) = (len col2))
    Seq.zip (sort col1) (sort col2)
    |> Seq.map (fun x -> abs(fst x - snd x))
    |> Seq.sum

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
