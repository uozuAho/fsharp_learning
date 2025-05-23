#load "MyUtils.fsx"
open MyUtils

type Grid =
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
    member this.show =
        for row in this.cells do
            let str = new System.String(row |> List.toArray)
            printfn $"{str}"
    member this.inBounds p =
        let x, y = p
        this.inBoundsXy x y
    member this.inBoundsXy x y =
        x >= 0 && x < this.width
        && y >= 0 && y < this.height

let fromLines lines =
    let chars = lines |> map str2chars |> Seq.toList
    {
        cells = chars
        height = chars.Length
        width = chars.[0].Length
    }

let fromStr (str:string) =
    str.Split('\n') |> Array.where ((<>) "") |> fromLines

let fromChars height width xy2char =
    [for row in [0..height - 1] do
        new System.String([|for col in [0..width - 1] -> xy2char col row|])]
    |> fromLines

let show (grid:Grid) = grid.show
