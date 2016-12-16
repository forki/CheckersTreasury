namespace Checkers

open Checkers.Types
open Checkers.Piece

module public Board =

    type Board = Piece option list list

    let row = List.item
    let square coord = List.item coord.Row >> List.item coord.Column

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