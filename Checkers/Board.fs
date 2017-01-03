module public Checkers.Board
open Checkers.Types
open Checkers.Piece
open System.Linq

type Board = Piece option list list

let square (coord :Coord) = List.item coord.Row >> List.item coord.Column

let rowFromSeq (value :'a seq) =
    Some (List.ofSeq value)

let listFromSeq (value :'a seq seq) =
    List.ofSeq (Seq.choose rowFromSeq value)

let defaultBoard =
    [
        List.replicate 4 [None; blackChecker] |> List.concat
        List.replicate 4 [blackChecker; None] |> List.concat
        List.replicate 4 [None; blackChecker] |> List.concat
        List.replicate 8 None
        List.replicate 8 None
        List.replicate 4 [whiteChecker; None] |> List.concat
        List.replicate 4 [None; whiteChecker] |> List.concat
        List.replicate 4 [whiteChecker; None] |> List.concat
    ]
    
let defaultFen = "[FEN \"B:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,9,10,11,12\"]"

let emptyBoard =
    let row :Piece Option seq = Seq.replicate 8 None
    let board = Seq.replicate 8 row

    board.Select(fun item -> item.ToList()).ToList()