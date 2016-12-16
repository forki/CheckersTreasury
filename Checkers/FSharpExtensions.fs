namespace Checkers

open Checkers
open Checkers.Piece
open Checkers.Board
open Checkers.Types
open System
open System.Linq

module FSharpExtensions =

    let internal getJumpedCoord startCoord endCoord =
        { Row = startCoord.Row - Math.Sign(startCoord.Row - endCoord.Row); Column = startCoord.Column - Math.Sign(startCoord.Column - endCoord.Column) }

    let internal checkMoveDirection piece startCoord endCoord =
        match piece.PieceType with
        | PieceType.Checker ->
            match piece.Player with
            | Player.Black -> startCoord.Row < endCoord.Row
            | Player.White -> startCoord.Row > endCoord.Row
        | PieceType.King -> true

    let internal moveIsDiagonal startCoord endCoord =
        startCoord <> endCoord &&
        System.Math.Abs(startCoord.Row - endCoord.Row) = System.Math.Abs(startCoord.Column - endCoord.Column)

    let internal kingRowIndex(player) =
        match player with
        | Player.Black -> 7
        | Player.White -> 0
    
    let internal coordExists coord =
        coord.Row >= 0 && coord.Row <= 7 &&
        coord.Column >= 0 && coord.Column <= 7

    let internal isValidCheckerHop startCoord endCoord (board :Board) =
        let piece = (square startCoord board).Value

        checkMoveDirection piece startCoord endCoord &&
        (square startCoord board).IsSome &&
        (square endCoord board).IsNone

    let internal isValidKingHop startCoord endCoord (board :Board) =
        (square startCoord board).IsSome &&
        (square endCoord board).IsNone

    let internal isValidCheckerJump startCoord endCoord (board :Board) =
        let piece = (square startCoord board).Value
        
        let jumpedCoord = getJumpedCoord startCoord endCoord
        let jumpedPiece = square jumpedCoord board
        
        checkMoveDirection piece startCoord endCoord &&
        (square startCoord board).IsSome &&
        (square endCoord board).IsNone &&
        jumpedPiece.IsSome &&
        jumpedPiece.Value.Player <> piece.Player

    let internal isValidKingJump startCoord endCoord (board :Board) =
        let piece = (square startCoord board).Value

        let jumpedCoord = getJumpedCoord startCoord endCoord
        let jumpedPiece = square jumpedCoord board

        (square endCoord board).IsNone &&
        (square startCoord board).IsSome &&
        jumpedPiece.IsSome &&
        jumpedPiece.Value.Player <> piece.Player
            
    let internal isValidHop startCoord endCoord (board :Board) =
        match (square startCoord board).Value.PieceType with
        | PieceType.Checker -> isValidCheckerHop startCoord endCoord board
        | PieceType.King -> isValidKingHop startCoord endCoord board
        
    let internal isValidJump startCoord endCoord (board :Board) =
        match (square startCoord board).Value.PieceType with
        | PieceType.Checker -> isValidCheckerJump startCoord endCoord board
        | PieceType.King -> isValidKingJump startCoord endCoord board

    let internal hasValidJump startCoord (board :Board) =
        let jumpUpRightCoord = startCoord + {Row = -2; Column = 2}
        let jumpUpLeftCoord = startCoord + {Row = -2; Column = -2}
        let jumpDownRightCoord = startCoord + {Row = 2; Column = 2}
        let jumpDownLeftCoord = startCoord + {Row = 2; Column = -2}

        let jumpUpRightValid = coordExists jumpUpRightCoord && isValidJump startCoord jumpUpRightCoord board
        let jumpUpLeftValid = coordExists jumpUpLeftCoord && isValidJump startCoord jumpUpLeftCoord board
        let jumpDownRightValid = coordExists jumpDownRightCoord && isValidJump startCoord jumpDownRightCoord board
        let jumpDownLeftValid = coordExists jumpDownLeftCoord && isValidJump startCoord jumpDownLeftCoord board

        jumpUpRightValid || jumpUpLeftValid ||
        jumpDownRightValid || jumpDownLeftValid

    let internal jumpAvailable player (board :Board) =
        board.Select(fun row rowIndex -> row.Select(fun item colIndex -> item.IsSome && item.Value.Player = player && hasValidJump { Row = rowIndex; Column = colIndex } board).Any(fun item -> item)).Any(fun item -> item)

    let internal setPieceAt coord piece (board :Board) =
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

        boardItems

    let internal jump startCoord endCoord (board :Board) =
        let kingRowIndex = kingRowIndex((square startCoord board).Value.Player)

        let piece =
            match endCoord.Row with
            | row when row = kingRowIndex -> Some <| Piece.Promote (square startCoord board).Value
            | _ -> (square startCoord board)

        let jumpedCoord = getJumpedCoord startCoord endCoord

        board
        |> setPieceAt startCoord None
        |> setPieceAt endCoord piece
        |> setPieceAt jumpedCoord None

    let internal hop startCoord endCoord (board :Board) =
        let kingRowIndex = kingRowIndex (square startCoord board).Value.Player

        let piece =
            match endCoord.Row with
            | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
            | _ -> (square startCoord board)
            
        board
        |> setPieceAt startCoord None
        |> setPieceAt endCoord piece