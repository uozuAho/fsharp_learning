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
#load "CharGrid.fsx"

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

// defaultdict may be handy here. No standard impl
let findAllAntennaPos (grid:CharGrid.Grid) =
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

let withAntis (grid:CharGrid.Grid) antis =
    let xy2char x y =
        let gridchar = grid.charAt x y
        if gridchar <> '.' then gridchar
        else if Seq.contains (x, y) antis then '#'
        else '.'
    CharGrid.fromChars grid.height grid.width xy2char

let solve1 (lines:seq<string>) =
    let grid = lines |> CharGrid.fromLines
    let antis = genAllAntinodes grid |> where grid.inBounds
    printfn $"{antis |> Seq.length}"
    // grid.show
    // CharGrid.show (withAntis grid antis)

(*
part 2 : antinodes are created repeatedly in line, not just once
*)

let antiPosRepeat pos pos2 (grid:CharGrid.Grid) =
    (pos, pos2)
    |> Seq.unfold (fun ps ->
        let p1, p2 = ps
        if not (grid.inBounds p1) then None
        else Some(ps, (p2, antiPos p1 p2)))
    |> map fst

let genAntinodesRepeat poss grid =
    seq {
        for pair in Seq.allPairs poss poss do
            let p1 = fst pair
            let p2 = snd pair
            if p1 <> p2 then yield! antiPosRepeat p1 p2 grid
    }

let genAllAntinodesRepeat grid =
    let ants = findAllAntennaPos grid
    let antis = seq {
        for group in ants do
            let poss = snd group
            yield! (genAntinodesRepeat poss grid)
    }
    antis |> Seq.distinct

let solve2 lines =
    let grid = lines |> CharGrid.fromLines
    let allantis = genAllAntinodesRepeat grid |> where grid.inBounds
    printfn $"{allantis |> Seq.length}"
    // grid.show
    // CharGrid.show (withAntis grid antis)

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
