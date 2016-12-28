module public Checkers.PublicAPI
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.Minimax
open Checkers.GameController

let isValidMove startCoord endCoord gameController =
    isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let movePiece startCoord endCoord gameController :Option<GameController> =
    let board = movePiece startCoord endCoord gameController.Board

    match (isValidMove startCoord endCoord gameController) with
    | false -> None
    | true ->
        Some <|
            {
                Board = board.Value;
                CurrentPlayer = match playerTurnEnds [startCoord; endCoord] gameController.Board board.Value with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer        
                CurrentCoord = match playerTurnEnds [startCoord; endCoord] gameController.Board board.Value with
                                | true -> None
                                | false -> Some endCoord
            }

let move (move :Coord seq) (gameController) :Option<GameController> =
    let board = moveSequence move (Some gameController.Board)
    match board with
    | None -> None
    | Some b ->
        Some <|
            {
                Board = board.Value;
                CurrentPlayer = match playerTurnEnds (List.ofSeq move) gameController.Board board.Value with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer        
                CurrentCoord = match playerTurnEnds (List.ofSeq move) gameController.Board board.Value with
                                | true -> None
                                | false -> Some (Seq.last move)
            }

let getMove searchDepth gameController =
    (minimax gameController.CurrentPlayer searchDepth None None gameController.Board).Move

let isWon controller =
    isWon controller.Board