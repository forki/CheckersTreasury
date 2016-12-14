namespace Checkers

type Piece(player:Player, pieceType:PieceType) =
    member this.Player = player
    member this.PieceType = pieceType

    member this.Promote() =
        new Piece(player, PieceType.King)

    override this.Equals (obj) =
        let piece = obj :?> Piece
        piece.Player = this.Player &&
        piece.PieceType = this.PieceType

    override this.GetHashCode() =
        this.Player.GetHashCode() ^^^ this.PieceType.GetHashCode()

    static member WhiteChecker() =
        Some <| new Piece(Player.White, PieceType.Checker)

    static member BlackChecker() =
        Some <| new Piece(Player.Black, PieceType.Checker)

    static member WhiteKing() =
        Some <| new Piece(Player.White, PieceType.King)

    static member BlackKing() =
        Some <| new Piece(Player.Black, PieceType.King)