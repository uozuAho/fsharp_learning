(*
https://adventofcode.com/2024/day/6

part 2
- find all single positions you could add an obstacle to cause the guard to loop
- return count of positions
- loop: guard returns to a previous state
- no loop: guard leaves map

brute force idea
for each original visited position
    place an obstacle there
        run sim, check for loop
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
    let guardChar =
        match dir with
        | Up -> '^'
        | Down -> 'v'
        | Left -> '<'
        | Right -> '>'
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

let runToEnd mapStr =
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
    let guard, map =
        match fromStr mapStr with
        | Some(g, m) -> g, m
        | None -> failwith "invalid map"
    run guard map [] []

let map = "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"
// let map = File.ReadAllText "input.txt"

let prevStates, visited, isLoop = runToEnd map
printfn "%A" visited.Length
// tests.run
