module public Checkers.PublicAPI
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open Checkers.GameController

let isValidMove startCoord endCoord gameController =
    isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

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

let getMove gameController =
    getBestMove gameController.CurrentPlayer gameController.Board

let isWon controller =
    match (moveAvailable controller.Board) with
    | x when not <| x White -> Some Black
    | x when not <| x Black -> Some White
    | _ -> None