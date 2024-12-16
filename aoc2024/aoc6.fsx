(*
https://adventofcode.com/2024/day/6

unfinished, doesn't get the right result for part 2
*)

#load "MyUtils.fsx"
#load "ScriptTest.fsx"
open MyUtils;
open ScriptTest;
open System.IO

let tests = Tests()

type CellItem = Empty | Obstacle
type Row = list<CellItem>
type Pos = int * int
type Map =
    {
        Height: int
        Width: int
        Obstacles: list<Pos>
    }
    member m.inBounds pos =
        let x, y = pos
        x >= 0 && x < m.Width
        && y >= 0 && y < m.Height
type Direction =
    Up | Down | Left | Right
    member x.turnRight =
        match x with
        | Up -> Right
        | Right -> Down
        | Down -> Left
        | Left -> Up
    member x.vec =
        match x with
        | Up -> 0, -1
        | Right -> 1, 0
        | Down -> 0, 1
        | Left -> -1, 0
type Guard = Pos * Direction


let isObstacle (map:Map) pos =
    List.contains pos map.Obstacles

let step pos (dir:Direction) =
    let x, y = pos
    let dirx, diry = dir.vec
    x + dirx, y + diry

let next guard map =
    let pos, dir = guard
    let posAhead = step pos dir
    let obstacleAhead = isObstacle map posAhead
    let newPos = if obstacleAhead then pos else posAhead
    let newDir = if obstacleAhead then dir.turnRight else dir
    newPos, newDir

let char2dir c =
    match c with
    | '^' -> Some(Up)
    | '>' -> Some(Right)
    | 'v' -> Some(Down)
    | '<' -> Some(Left)
    | _ -> None

let dir2char d =
    match d with
    | Up -> '^'
    | Down -> 'v'
    | Left -> '<'
    | Right -> '>'

let fromStr (str:string) =
    let rows = str.Split('\n') |> Array.where ((<>) "")
    let len0 = rows.[0].Length
    if not (Array.forall (fun (x:string) -> x.Length = len0) rows)
        then None
    else
        let height = rows.Length
        let width = rows.[0].Length
        let obstacles = [for y, row in enumerate rows do
                           for x, char in enumerate row do
                                if char = '#' then (x, y)]
        let guards = [for y, row in enumerate rows do
                        for x, char in enumerate row do
                            if "^>v<".Contains(char) then
                                let dir = char2dir char
                                match dir with
                                | Some(d) -> Some((x, y), d)
                                | _ -> None] |> List.choose id
        match guards with
        | guard::[] ->
            let map = {
                Height = height
                Width = width
                Obstacles = obstacles
            }
            Some(guard, map)
        | _ -> None

let tostr guard (map:Map) =
    let guardPos, dir = guard
    let guardChar = dir2char dir
    let chars =
        [for y in [0..map.Height - 1] do
            for x in [0..map.Width - 1] do
                let pos = x, y
                if pos = guardPos then guardChar
                else if (isObstacle map (x, y)) then '#' else '.'
            '\n']
    chars |> List.toArray |> System.String

let incr map =
    let guard, map =
        match fromStr map with
        | Some(g, m) -> g, m
        | None -> failwith "invalid map"
    let guard = next guard map
    tostr guard map

let runToEnd guard map =
    let rec run guard (map:Map) prevStates visited =
        let isLoop = List.contains guard prevStates
        let guardLeftMap =
            let pos, _ = guard
            not (map.inBounds pos)
        let terminated = isLoop || guardLeftMap
        if terminated then prevStates, visited, isLoop
        else
            let prevStates = guard::prevStates
            let visited =
                let pos, _ = guard
                if (List.contains pos visited)
                then visited else pos::visited
            run (next guard map) map prevStates visited
    run guard map [] []

(*
part 2
- find all single positions you could add an obstacle to cause the guard to loop
- return count of positions
- loop: guard returns to a previous state
- no loop: guard leaves map

brute force idea
for each original visited position
    place an obstacle there
        run sim, check for loop

assumptions: original map can loop, we still need to add an obstacle
*)

// slow: ~4 checks/second for total of 5000
let findLoopPossBrute guard map =
    let originalMap = map
    let _, visited, _ = runToEnd guard originalMap

    printfn $"visited: {visited.Length}"
    let mutable counter = 0

    seq {
        for pos in visited do
        let newMap = { originalMap with Obstacles = pos::originalMap.Obstacles }
        let _, _, isLoop = runToEnd guard newMap
        counter <- counter + 1
        printfn $"checked {counter}"
        if isLoop then yield pos
    }

(*
idea2: search by obstacle instead of each guard pos
fun nextObstacle guard obstacles -> pos|None

for each original visited position
    place obstacle there
        let visited_obs = []
        let terminated = false
        while not terminated
            let next = nextObstacle guard obs
            terminated = next in visited_obs | next is None
*)
let nextObstacle guard obstacles =
    let willHit guard obstacle =
        let (x, y), dir = guard
        let ox, oy = obstacle
        match dir with
        | Up -> x = ox && y > oy
        | Down -> x = ox && y < oy
        | Left -> y = oy && x > ox
        | Right -> y = oy && x < ox
    obstacles |> List.tryFind (willHit guard)

let nextBlockedGuard guard obstacles =
    let nextObs = nextObstacle guard obstacles
    match nextObs with
    | None -> None
    | Some(o) ->
        let x, y = o
        let _, dir = guard
        match dir with
        | Up -> Some((x, y + 1), Up)
        | Down -> Some((x, y - 1), Down)
        | Left -> Some((x + 1, y), Left)
        | Right -> Some((x - 1, y), Right)

let doesLoop guard map =
    let rec run guard (map:Map) prevStates =
        let isLoop = List.contains guard prevStates
        let guardLeftMap =
            let pos, _ = guard
            not (map.inBounds pos)
        let terminated = isLoop || guardLeftMap
        if terminated then isLoop
        else
            let nextGuard = nextBlockedGuard guard map.Obstacles
            match nextGuard with
            | None -> false
            | Some(g) ->
                let prevStates = guard::prevStates
                let pos, dir = g
                let newGuard = pos, dir.turnRight
                run newGuard map prevStates
    run guard map []

let findLoopPossNextObs guard map =
    let originalMap = map
    let _, visited, _ = runToEnd guard originalMap

    printfn $"visited: {visited.Length}"
    let mutable counter = 0

    seq {
        for pos in visited do
        let newMap = { originalMap with Obstacles = pos::originalMap.Obstacles }
        let isLoop = doesLoop guard newMap
        counter <- counter + 1
        // printfn $"checked {counter}"
        if isLoop then yield pos
    }


// let mapStr = "
// ....#.....
// .........#
// ..........
// ..#.......
// .......#..
// ..........
// .#..^.....
// ........#.
// #.........
// ......#...
// "
// let mapStr = "
// .#.
// #.#
// .^.
// "
// let mapStr = "
// .#..
// ...#
// .^..
// ..#.
// "
let mapStr = File.ReadAllText "input.txt"

let guard, map =
    match fromStr mapStr with
    | Some(g, m) -> g, m
    | None -> failwith "invalid map"

// let prevStates, visited, isLoop = runToEnd guard map
// printfn "%A" visited.Length
// let loopPos = findLoopPossBrute guard map // slow
let loopPos = findLoopPossNextObs guard map
printfn "%A" loopPos
printfn "%d" (loopPos |> Seq.toList |> List.length)
// tests.run

open System

let printc color (text:string) =
    let originalColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    Console.Write text
    Console.ForegroundColor <- originalColor

let drawCli guard (map:Map) visited =
    let gpos, gdir = guard
    let gchar = dir2char gdir
    printfn ""
    for y in [0..map.Height-1] do
        for x in [0..map.Width-1] do
            if (x, y) = gpos
                then printc ConsoleColor.Red (string gchar)
            else if List.contains (x, y) visited
                then printc ConsoleColor.Blue "o"
            else if isObstacle map (x, y)
                then printc ConsoleColor.Gray "#"
            else
                printc ConsoleColor.Gray "."
        printfn ""

// let pos, dir = guard
// let mutable mGuard = guard
// let mutable mVisited = [pos]

// to use this in fsi, call `cliLoop &mGuard map &mVisited;;`
// let cliLoop (guard:byref<_>) map (visited:byref<_>) ff =
//     drawCli guard map visited
//     let advance =
//         guard <- next mGuard map
//         let pos, _ = mGuard
//         visited <- pos::visited
//     let rec advanceFF pGuard guard visited =
//         let _, pdir = pGuard
//         let _, dir = guard
//         if dir <> pdir then
//             advanceFF guard
//     if ff then
//         advanceFF guard

// can this work without mutation?
let cliLoop pguard guard map visited ff =
    drawCli guard map visited
    let advance guard visited =
        let nguard = next guard map
        let pos, _ = guard
        guard, nguard, pos::visited
    let rec advanceFF pGuard guard visited =
        let _, pdir = pGuard
        let pos, dir = guard
        match map.inBounds pos, dir = pdir with
        | false, _ -> pGuard, guard, visited
        | true, true ->
            let _, ng, nv = advance guard visited
            advanceFF guard ng nv
        | true, false ->
            advance guard visited
    if ff then
        advanceFF pguard guard visited
    else
        advance guard visited

let nextGuard = next guard map
let visited = []

// to use in fsi:
// let guard, nextGuard, visited = cliLoop guard nextGuard map visited true;;
