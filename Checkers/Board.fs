module public Checkers.Board
open Checkers.Types
open Checkers.Piece

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