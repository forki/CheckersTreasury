namespace Checkers

type Board(board) =
    new () = Board(Board.DefaultBoard)

    member this.Board :List<List<Option<Piece>>> = board

    member this.Item
        with get(coord :Coord) =
            this.Board.[coord.Row].[coord.Column]

    member this.Item
        with get(row :int) =
            this.Board.[row]

    static member DefaultBoard =
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