namespace Checkers

open Checkers.Types

module public Piece =
    type Piece = { Player :Player; PieceType :PieceType }

    let Promote piece = { Player = piece.Player; PieceType = King }

    let whiteChecker = Some <| { Player = White; PieceType = Checker }
    let whiteKing = Some <| { Player = White; PieceType = King }

    let blackChecker = Some <| { Player = Black; PieceType = Checker }
    let blackKing = Some <| { Player = Black; PieceType = King }