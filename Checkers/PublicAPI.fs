namespace Checkers

open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameController
open System

module public PublicAPI =

    let isValidMove startCoord endCoord gameController =
        isValidMove startCoord endCoord gameController.Board &&
        (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
        match gameController.CurrentCoord with
        | None -> true
        | coord -> startCoord = coord.Value

    let private otherPlayer player =
        match player with
        | White -> Black
        | Black -> White

    let internal playerTurnEnds lastMoveStartCoord lastMoveEndCoord (originalBoard :Board) (currentBoard :Board) =
        let lastMoveWasJump = Math.Abs(lastMoveStartCoord.Row - lastMoveEndCoord.Row) = 2
        let pieceWasPromoted = (square lastMoveEndCoord currentBoard).Value.PieceType = King &&
                               (square lastMoveStartCoord originalBoard).Value.PieceType = Checker

        pieceWasPromoted ||
        not (lastMoveWasJump && hasValidJump lastMoveEndCoord currentBoard)

    let move startCoord endCoord gameController :Option<GameController> =
        let newBoard = movePiece startCoord endCoord gameController.Board

        match (isValidMove startCoord endCoord gameController) with
        | true -> Some <|
                  {
                      Board = newBoard.Value;
                      CurrentPlayer = match playerTurnEnds startCoord endCoord gameController.Board newBoard.Value with
                                      | true -> otherPlayer gameController.CurrentPlayer
                                      | false -> gameController.CurrentPlayer        
                      CurrentCoord = match playerTurnEnds startCoord endCoord gameController.Board newBoard.Value with
                                      | true -> None
                                      | false -> Some endCoord
                  }
        | false -> None

    let isWon controller =
        match moveAvailable with
        | x when not <| x White controller.Board -> Some Black
        | x when not <| x Black controller.Board -> Some White
        | _ -> None