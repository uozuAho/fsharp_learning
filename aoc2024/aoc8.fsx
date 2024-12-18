(*
https://adventofcode.com/2024/day/8

solve: dotnet fsi aoc8.fsx
repl:  dotnet fsi --define:DEBUG --use:aoc8.fsx

- grid
- antenna = [0-9a-zA-Z]
- antennas of same char create antinode '#' at 2 points in line with the antennae,
  at the same distance the antennae are from each other, eg # a a #, #  a  a  #
- antinodes can overlap antennae
*)

// quiet the REPL a bit. not perfect
fsi.ShowDeclarationValues <- false

#load "MyUtils.fsx"
open MyUtils

let sampleStr = "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
"

// todo: add these to utils
type CharGrid =
    {
        cells: list<list<char>>
        height: int
        width: int
    }
    member this.charAt x y =
        // todo: bounds check
        this.cells.[y].[x]
    member this.allCells =
        seq {
            for rowNum, row in enumerate this.cells do
                for colNum, char in enumerate row do
                    ((colNum, rowNum), char)
        }
    member this.toString =
        for row in this.cells do
            let str = new System.String(row |> List.toArray)
            printfn $"{str}"

let asChars (str:string) =
    str.ToCharArray() |> Array.toList

let toGrid lines =
    let chars = lines |> map asChars |> Seq.toList
    {
        cells = chars
        height = chars.Length
        width = chars.[0].Length
    }

let distTo pos1 pos2 =
    let x1, y1 = pos1
    let x2, y2 = pos2
    x2 - x1, y2 - y1

let xyop op pos1 pos2 =
    let x1, y1 = pos1
    let x2, y2 = pos2
    op x1 x2, op y1 y2

let antiPos pos1 pos2 =
    let x, y = distTo pos1 pos2
    xyop (+) pos2 (x, y)

let findAllAntennaPos (grid:CharGrid) =
    grid.allCells
    |> Seq.where (fun x -> snd x <> '.')
    |> map (fun x -> snd x, fst x)
    |> Seq.groupBy fst
    |> map (fun x ->
        let key = fst x
        let poss = snd x |> map snd |> Seq.toList
        key, poss)

let genAntinodes poss =
    seq {
        for pair in Seq.allPairs poss poss do
            let p1 = fst pair
            let p2 = snd pair
            if p1 <> p2 then antiPos p1 p2
    }

let genAllAntinodes grid =
    let ants = findAllAntennaPos grid
    let antis = seq {
        for group in ants do
            let poss = snd group
            yield! (genAntinodes poss)
    }
    antis |> Seq.distinct

let addAntis grid antis =
    let lines = seq {
        for row in [0..grid.height - 1] do
            let rowchars = seq {
                for col in [0..grid.width - 1] do
                    let gridchar = grid.charAt col row
                    if grid.charAt col row <> '.' then gridchar
                    else if Seq.contains (col, row) antis then '#'
                    else '.'
            }
            let strarr = rowchars |> Seq.toArray
            new System.String(strarr)
    }
    toGrid lines

let solve1 (lines:seq<string>) =
    let map = lines |> map (fun s -> s.ToCharArray()) |> Seq.toList
    // todo: parse grid to 2d array
    // todo: easier string to chars


    // example usage of some of my utils. remove once you've done a few more puzzles
    // let numberLines = linesAsNumbers lines
    // let col1 = numberLines |> map (el 0)
    // let col2 = numberLines |> map (el 1)
    // wozAssert ((len col1) = (len col2))
    // Seq.zip (sort col1) (sort col2)
    // |> Seq.map (fun x -> abs(fst x - snd x))
    // |> Seq.sum
    0

let solve2 lines =
    0

// todo: this needs session token to work
// manually save to input.txt for now
// let realInput = aocFetchInput 2024 7
let input = System.IO.File.ReadAllLines "input.txt"
let sample = toLines sampleStr

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

let grid = sample |> toGrid
let allantis = genAllAntinodes grid
printfn $"{allantis |> Seq.length}"
let fullGrid = addAntis grid allantis
printfn $"{fullGrid.toString}"
// todo: grid looks right, I count 13 # + an A in the right spot
//       why is allantis len = 17??
// todo: useful utils: grid from char list, grid.show (print)
