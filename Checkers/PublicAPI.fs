module public Checkers.PublicAPI
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
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

let rec getMove player (searchDepth :int) (board :Board) =
    let moves = calculateMoves player board

    let wonBoards = List.map (fun x -> (isWon (moveSequence x (Some board)).Value).IsSome) moves

    let opponentMoves =
        match searchDepth = 0 || List.exists id wonBoards with
        | false -> List.map (fun x -> getMove (otherPlayer player) (searchDepth - 1) (moveSequence x (Some board)).Value) moves
        | true -> List.empty

    let weightedMoves = List.mapi (fun i m -> (calculateWeight player (match (opponentMoves.IsEmpty) with
                                                                       | true -> (moveSequence m (Some board)).Value
                                                                       | false -> let newBoard = (moveSequence m (Some board)).Value
                                                                                  (moveSequence opponentMoves.[i] (Some newBoard)).Value),
                                                                       m)) moves

    let rec loop highestWeight moveForHighestWeight (list :List<float * Move>) =
        let weight = fst list.Head
        let newMoveForHighestWeight =
            match weight >= highestWeight with
            | true -> snd list.Head
            | false -> moveForHighestWeight

        match list.Tail.IsEmpty with
        | false -> loop (Math.Max(highestWeight, weight)) newMoveForHighestWeight list.Tail
        | true -> newMoveForHighestWeight

    loop (fst weightedMoves.Head) (snd weightedMoves.Head) weightedMoves

let isWon controller =
    isWon controller.Board