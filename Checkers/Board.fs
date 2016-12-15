namespace Checkers

open System.Collections.Generic
open System.Linq

type Board(board) =
    new () = Board(Board.DefaultBoard)

    new(board :IEnumerable<IEnumerable<Option<Piece>>>) =
        let boardArray = List.ofSeq(board.Select(fun r -> List.ofSeq(r)))
        Board(boardArray)

    member this.Board :Option<Piece> list list = board

    member this.Item
        with get(coord :Coord) =
            this.Board.[coord.Row].[coord.Column]

    member this.Item
        with get(row :int) =
            this.Board.[row]

    static member DefaultBoard :Option<Piece> list list =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];