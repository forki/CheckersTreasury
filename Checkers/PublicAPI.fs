namespace Checkers

open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameController
open System

module public PublicAPI =

    let isValidMove startCoord endCoord gameController =
        isValidMove startCoord endCoord gameController.Board &&
        (square startCoord gameController.Board).Value.Player = gameController.Player

    let private otherPlayer player =
        match player with
        | White -> Black
        | Black -> White

    let internal playerTurnEnds lastMoveStartCoord lastMoveEndCoord currentBoard =
        let lastMoveWasJump = Math.Abs(lastMoveStartCoord.Row - lastMoveEndCoord.Row) = 2

        not (lastMoveWasJump && hasValidJump lastMoveEndCoord currentBoard)

    let move startCoord endCoord gameController :Option<GameController> =
        let newBoard = movePiece startCoord endCoord gameController.Board

        match (isValidMove startCoord endCoord gameController) with
        | true -> Some <|
                  {
                      Board = newBoard.Value;
                      Player = match playerTurnEnds startCoord endCoord newBoard.Value with
                               | true -> otherPlayer gameController.Player
                               | false -> gameController.Player
                  }
        | false -> None

    let isWon controller =
        match moveAvailable with
        | x when not <| x White controller.Board -> Some Black
        | x when not <| x Black controller.Board -> Some White
        | _ -> None