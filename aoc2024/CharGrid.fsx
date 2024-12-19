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
    member this.toString =
        for row in this.cells do
            let str = new System.String(row |> List.toArray)
            printfn $"{str}"
    member this.inBounds p =
        let x, y = p
        this.inBoundsXy x y
    member this.inBoundsXy x y =
        x >= 0 && x < this.width
        && y >= 0 && y < this.height


// todo: useful utils: grid from char list, grid.show (print)
