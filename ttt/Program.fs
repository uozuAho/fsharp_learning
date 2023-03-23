type ROW = TOP | RMIDDLE | BOTTOM
type COL = LEFT | CMIDDLE | RIGHT
type TOKEN = X | O | EMPTY
type POS = ROW * COL
type CELL = POS * TOKEN

type Board = {
    Cells: CELL list
    Turn: TOKEN
}

// This was written by copilot ... I don't really get it
let xHasWon board =
    let xCells = board.Cells |> List.filter (fun (_, t) -> t = X)
    let xRows = xCells |> List.map (fun ((r, _), _) -> r)
    let xCols = xCells |> List.map (fun ((_, c), _) -> c)
    let xDiag1 = xCells |> List.map (fun ((r, c), _) -> (r, c)) |> List.contains (TOP, LEFT)
    let xDiag2 = xCells |> List.map (fun ((r, c), _) -> (r, c)) |> List.contains (TOP, RIGHT)
    let xHasWonRow = xRows |> List.contains TOP && xRows |> List.contains RMIDDLE && xRows |> List.contains BOTTOM
    let xHasWonCol = xCols |> List.contains LEFT && xCols |> List.contains CMIDDLE && xCols |> List.contains RIGHT
    xHasWonRow || xHasWonCol || xDiag1 || xDiag2

let other = function
    | X -> O
    | O -> X
    | _ -> failwith "Invalid token"
    
let cellPos (pos, _) = pos

let place cell board =
    let replaceMatchingCell oldCell =
        if cellPos oldCell = cellPos cell then cell else oldCell
    let newCells = board.Cells |> List.map replaceMatchingCell
    { Cells = newCells; Turn = other board.Turn }
    

// -------------------------------    
// tests
open Xunit

let EmptyCells () = [
    (TOP, LEFT), EMPTY
    (TOP, CMIDDLE), EMPTY
    (TOP, RIGHT), EMPTY
    (RMIDDLE, LEFT), EMPTY
    (RMIDDLE, CMIDDLE), EMPTY
    (RMIDDLE, RIGHT), EMPTY
    (BOTTOM, LEFT), EMPTY
    (BOTTOM, CMIDDLE), EMPTY
    (BOTTOM, RIGHT), EMPTY
]

let EmptyBoard () = { Cells = EmptyCells(); Turn = X }

let strToBoard (str:string) =
    let charToToken = function
        | 'x' -> X
        | 'o' -> O
        | '.' -> EMPTY
        | _ -> failwith "Invalid token"

    let strToTokens (str:string) =
        let validToken = fun c -> c = 'x' || c = 'o' || c = '.'
        String.filter validToken str |> Seq.map charToToken
        
    let idxToPos = function
        | 1 -> (TOP, LEFT)
        | 2 -> (TOP, CMIDDLE)
        | 3 -> (TOP, RIGHT)
        | 4 -> (RMIDDLE, LEFT)
        | 5 -> (RMIDDLE, CMIDDLE)
        | 6 -> (RMIDDLE, RIGHT)
        | 7 -> (BOTTOM, LEFT)
        | 8 -> (BOTTOM, CMIDDLE)
        | 9 -> (BOTTOM, RIGHT)
        | _ -> failwith "Invalid idx"
        
    let numXs = String.filter (fun c -> c = 'x') str |> String.length
    let numOs = String.filter (fun c -> c = 'o') str |> String.length
    let turn = if numXs = numOs then X else O

    let tokens = strToTokens str
    let cells = Seq.zip tokens [1..9] |> Seq.map (fun (t, i) -> (idxToPos i, t)) |> Seq.toList
    { Cells = cells; Turn = turn }
    
let boardToStr board =
    let tokenToChar = function
        | X -> "x"
        | O -> "o"
        | EMPTY -> "."
    let cellToChar cell = cell |> snd |> tokenToChar
    board.Cells |> Seq.map cellToChar |> String.concat ""

[<Fact>]
let ``string to board`` () =
    let str = "xx.o..o.."
    let board = strToBoard str
    let expectedBoard = {
        Cells = [
            ((TOP, LEFT), X)
            ((TOP, CMIDDLE), X)
            ((TOP, RIGHT), EMPTY)
            ((RMIDDLE, LEFT), O)
            ((RMIDDLE, CMIDDLE), EMPTY)
            ((RMIDDLE, RIGHT), EMPTY)
            ((BOTTOM, LEFT), O)
            ((BOTTOM, CMIDDLE), EMPTY)
            ((BOTTOM, RIGHT), EMPTY)
        ]
        Turn = X
    }
    Assert.Equal(expectedBoard, board)

[<Fact>]
let ``multiline string to board`` () =
    let str = "xx.
               o..
               o.."
    let board = strToBoard str
    let expectedBoard = {
        Cells = [
            ((TOP, LEFT), X)
            ((TOP, CMIDDLE), X)
            ((TOP, RIGHT), EMPTY)
            ((RMIDDLE, LEFT), O)
            ((RMIDDLE, CMIDDLE), EMPTY)
            ((RMIDDLE, RIGHT), EMPTY)
            ((BOTTOM, LEFT), O)
            ((BOTTOM, CMIDDLE), EMPTY)
            ((BOTTOM, RIGHT), EMPTY)
        ]
        Turn = X
    }
    Assert.Equal(expectedBoard, board)
    
[<Fact>]
let ``string to board, O's turn`` () =
    let str = "xx.o....."
    let board = strToBoard str
    Assert.Equal(O, board.Turn)
    
[<Fact>]
let ``place first token`` () =
    let board = EmptyBoard()
    let newBoard = place ((TOP, LEFT), X) board
    Assert.Equal("x........", boardToStr newBoard)
    
[<Fact>]
let ``x wins when all top row`` () =
    let board = EmptyBoard()
    let newBoard =
        board
        |> place ((TOP, LEFT), X)
        |> place ((RMIDDLE, LEFT), O)
        |> place ((TOP, CMIDDLE), X)
        |> place ((RMIDDLE, CMIDDLE), O)
        |> place ((TOP, RIGHT), X)
    Assert.Equal("xxxoo....", boardToStr newBoard)
    
    
[<Fact>]
let ``x wins when diagonal`` () =
    let board = EmptyBoard()
    let newBoard =
        board
        |> place ((TOP, LEFT), X)
        |> place ((RMIDDLE, LEFT), O)
        |> place ((RMIDDLE, CMIDDLE), X)
        |> place ((RMIDDLE, RIGHT), O)
        |> place ((BOTTOM, RIGHT), X)
    Assert.Equal("x..oxo..x", boardToStr newBoard)
    Assert.True(xHasWon newBoard)
    
[<Fact>]
let ``x wins when reverse diagonal`` () =
    let board = EmptyBoard()
    let newBoard =
        board
        |> place ((TOP, RIGHT), X)
        |> place ((RMIDDLE, LEFT), O)
        |> place ((RMIDDLE, CMIDDLE), X)
        |> place ((RMIDDLE, RIGHT), O)
        |> place ((BOTTOM, LEFT), X)
    Assert.Equal("..xoxox..", boardToStr newBoard)
    Assert.True(xHasWon newBoard)