module public Checkers.Board
open Checkers.Types
open Checkers.Piece
open System.Collections.Generic

type Board = Piece option list list

let square (coord :Coord) = List.item coord.Row >> List.item coord.Column

let rec rowFromSeq (value :'a IEnumerable) =
    Some (List.ofSeq value)

let rec listFromSeq (value :'a IEnumerable IEnumerable) =
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