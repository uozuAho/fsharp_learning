open System.IO
(*
https://adventofcode.com/2024/day/4

part 1: XMAS word search. Can be horiz, vert, diag, backwards

// todo: get chatty to solve part one, compare

part 2: X-MAS word search: MAS in an X, eg
M.S
.A.
M.S

Can be forwards or backwards.
*)

type Direction = Up | Down | Left | Right | UpRight | DownRight | UpLeft | DownLeft
type Board =
    // origin = top left
    { Letters: list<string> }
    member b.dimensions =
        (b.Letters.[0].Length, b.Letters.Length)
    member b.allPositions =
        let width, height = b.dimensions
        [for y in [0..height - 1] do
            for x in [0..width - 1] do (x, y)]
    member b.charAt pos =
        match b.bound pos with
        | Some(pos) ->
            let x, y = pos
            let line = b.Letters.[y]
            Some(line[x])
        | None -> None
    member b.nextPos pos dir =
        let addx, addy =
            match dir with
            | Up -> 0, -1
            | Down -> 0, 1
            | Left -> -1, 0
            | Right -> 1, 0
            | UpRight -> 1, -1
            | UpLeft -> -1, -1
            | DownRight -> 1, 1
            | DownLeft -> -1, 1
        let x, y = pos
        let nextX, nextY = x + addx, y + addy
        b.bound (nextX, nextY)
    member b.bound pos =
        let dimx, dimy = b.dimensions
        let x, y = pos
        match x, y with
        | a, b when a < 0 || b < 0 -> None
        | a, b when a >= dimx || b >= dimy -> None
        | a, b -> Some(a, b)

let all_directions = [Up; Down; Left; Right; UpRight; DownRight; UpLeft; DownLeft]


let str2board (str:string) =
    match str with
    | "" -> None
    | _ ->
        let letters =
            str.Split('\n')
            |> Array.where ((<>) "")
            |> Array.toList
        let len0 = letters.[0].Length
        let sameDim = letters |> List.forall (fun x -> x.Length = len0)
        if sameDim then
            Some({ Letters = letters })
        else
            None

let isXmasAt pos dir board =
    let rec asdf pos dir (word:string) (board:Board) =
        let char = board.charAt pos
        match char with
        | None -> false
        | Some(c) ->
            let word = word + string c
            match word with
            | "XMAS" -> true
            | "X" | "XM" | "XMA" ->
                let nextPos = board.nextPos pos dir
                match nextPos with
                | None -> false
                | Some(p) ->
                    asdf p dir word board
            | _ -> false
    asdf pos dir "" board

let findAllXmas (board:Board) =
    let xmasAtPosDir pos dir =
        if (isXmasAt pos dir board) then Some(pos, dir)
        else None

    [for pos in board.allPositions do
        for dir in all_directions do
            xmasAtPosDir pos dir] |> List.choose id

// part 2
let isMas9x9At pos (board:Board) =
    let rec wordAt word pos dir (board:Board) =
        if String.length word = 3 then Some(word)
        else
            let char = board.charAt pos
            match char with
            | None -> Some(word)
            | Some(c) ->
                let word = word + string c
                let nextPos = board.nextPos pos dir
                match nextPos with
                | None -> Some(word)
                | Some(p) -> wordAt word p dir board
    let word1 = wordAt "" pos DownRight board
    let x, y = pos
    let word2 =
        match board.charAt (x + 2, y) with
        | None -> None
        | Some(_) -> wordAt "" (x + 2, y) DownLeft board
    let ismas str = str = "MAS" || str = "SAM"
    match word1, word2 with
    | Some(x), Some(y) -> ismas x && ismas y
    | _ -> false


let countAllMas9x9 (board:Board) =
    [for pos in board.allPositions do isMas9x9At pos board]
    |> List.where id
    |> List.length

// -------------------------
// let myBoardStr = "....
// XMAS
// ....
// XMAS
// SAMX
// ...M
// ...A
// ...S"

let myBoardStr = File.ReadAllText("input.txt")

let myBoard = str2board myBoardStr
match myBoard with
| Some(board) ->
    let found = findAllXmas board
    printfn "%A" found
    printfn "%d" found.Length
    printfn "part2:"
    let count = countAllMas9x9 board
    printfn "%d" count
| None ->
    printfn "invalid board!"
