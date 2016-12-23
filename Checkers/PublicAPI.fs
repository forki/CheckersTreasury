module public Checkers.PublicAPI
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.Minimax
open Checkers.GameController
open System

let isValidMove startCoord endCoord gameController =
    isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let movePiece startCoord endCoord gameController :Option<GameController> =
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

let move (moves :System.Collections.Generic.IEnumerable<Coord>) (gameController) :Option<GameController> =
    let board = moveSequence moves (Some gameController.Board)
    match board with
    | Some b -> Some {Board = board.Value; CurrentPlayer = otherPlayer gameController.CurrentPlayer; CurrentCoord = gameController.CurrentCoord}
    | None -> None

let getMove searchDepth gameController =
    (minimax gameController.CurrentPlayer searchDepth Double.NegativeInfinity Double.PositiveInfinity gameController.Board).Move

let isWon controller =
    isWon controller.Board