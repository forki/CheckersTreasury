namespace Checkers

open Checkers
open System

module FSharpExtensions =

    let internal getJumpedCoord(startCoord, endCoord) =
        {Row = startCoord.Row - Math.Sign(startCoord.Row - endCoord.Row); Column = startCoord.Column - Math.Sign(startCoord.Column - endCoord.Column)}

    let internal checkMoveDirection(piece :Piece, startCoord :Coord, endCoord :Coord) =
        match piece.PieceType with
        | PieceType.Checker ->
            match piece.Player with
            | Player.Black -> startCoord.Row < endCoord.Row
            | Player.White -> startCoord.Row > endCoord.Row
        | PieceType.King -> true

    let internal moveIsDiagonal(startCoord :Coord, endCoord :Coord) =
        startCoord <> endCoord &&
        System.Math.Abs(startCoord.Row - endCoord.Row) = System.Math.Abs(startCoord.Column - endCoord.Column)

    let internal kingRowIndex(player) =
        match player with
        | Player.Black -> 7
        | Player.White -> 0
        
    type Board with
        member internal board.IsValidCheckerHop(startCoord :Coord, endCoord :Coord) =
            let piece = board.[startCoord].Value

            checkMoveDirection(piece, startCoord, endCoord) &&
            board.[endCoord].IsNone

    type Board with
        member internal board.IsValidKingHop(startCoord :Coord, endCoord :Coord) =
            board.[endCoord].IsNone

    type Board with
        member internal board.IsValidCheckerJump(startCoord :Coord, endCoord :Coord) =
            let piece = board.[startCoord].Value
        
            let jumpedCoord = getJumpedCoord(startCoord, endCoord)
            let jumpedPiece = board.[jumpedCoord]
        
            checkMoveDirection(piece, startCoord, endCoord) &&
            board.[endCoord].IsNone &&
            jumpedPiece.IsSome &&
            jumpedPiece.Value.Player <> piece.Player

    type Board with
        member internal board.IsValidKingJump(startCoord :Coord, endCoord :Coord) =
            let piece = board.[startCoord].Value

            let jumpedCoord = getJumpedCoord(startCoord, endCoord)
            let jumpedPiece = board.[jumpedCoord]

            board.[endCoord].IsNone &&
            jumpedPiece.IsSome &&
            jumpedPiece.Value.Player <> piece.Player
            
    type Board with
        member internal board.IsValidHop(startCoord :Coord, endCoord :Coord) =
            match board.[startCoord].Value.PieceType with
            | PieceType.Checker -> board.IsValidCheckerHop(startCoord, endCoord)
            | PieceType.King -> board.IsValidKingHop(startCoord, endCoord)
        
    type Board with
        member internal board.IsValidJump(startCoord :Coord, endCoord :Coord) =
            match board.[startCoord].Value.PieceType with
            | PieceType.Checker -> board.IsValidCheckerJump(startCoord, endCoord)
            | PieceType.King -> board.IsValidKingJump(startCoord, endCoord)

    type Board with
        member internal board.SetPieceAt(coord :Coord, piece :Option<Piece>) =
            let boardItems = List.init 8 (fun row ->
                match row with
                | i when i = coord.Row ->
                    List.init 8 (fun col ->
                        match col with
                        | j when j = coord.Column -> piece
                        | _ -> board.[row].[col]
                    )
                | _ -> board.[row]
            )
            new Board(boardItems)

    type Board with
        member internal board.Jump(startCoord :Coord, endCoord :Coord) =
            let kingRowIndex = kingRowIndex(board.[startCoord].Value.Player)

            let piece =
                match endCoord.Row with
                | row when row = kingRowIndex -> Some <| board.[startCoord].Value.Promote()
                | _ -> board.[startCoord]

            let jumpedCoord = getJumpedCoord(startCoord, endCoord)

            board.SetPieceAt(startCoord, None).SetPieceAt(endCoord, piece).SetPieceAt(jumpedCoord, None)

    type Board with
        member internal board.Hop(startCoord :Coord, endCoord :Coord) =
            let kingRowIndex = kingRowIndex(board.[startCoord].Value.Player)

            let piece =
                match endCoord.Row with
                | row when row = kingRowIndex -> Some <| board.[startCoord].Value.Promote()
                | _ -> board.[startCoord]

            board.SetPieceAt(startCoord, None).SetPieceAt(endCoord, piece)