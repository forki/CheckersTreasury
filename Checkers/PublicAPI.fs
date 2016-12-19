module public Checkers.PublicAPI
open Checkers.Variants
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
    let board = move moves (Some gameController.Board)
    match board with
    | Some b -> Some {Board = board.Value; CurrentPlayer = otherPlayer gameController.CurrentPlayer; CurrentCoord = gameController.CurrentCoord}
    | None -> None

let getMove searchDepth gameController =
    let rec loop (moves :(Coord * Coord) List) controller =
        let pieceMove = getBestMove controller.CurrentPlayer searchDepth controller.Board
        let startCoord, endCoord = pieceMove
        let newController = movePiece startCoord endCoord controller
        let newMoves = moves @ [pieceMove]
        
        match newController.IsSome && newController.Value.CurrentPlayer = controller.CurrentPlayer with
        | true -> loop newMoves newController.Value
        | false -> newMoves

    loop [] gameController

let isWon controller =
    isWon controller.Board