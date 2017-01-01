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

let internal getGameHistory (currentGameHistory :PDNTurn List) player move startBoard endBoard =
    let pdnMove = (List.map (fun item -> (square item PDNBoard).Value) move)
    let newTurnValue =
        match player with
        | Black ->
            let moveNumber = currentGameHistory.Length + 1
            {
                MoveNumber = moveNumber;
                BlackMove = { Move = pdnMove; PreviousFen = (CreateFen player startBoard); AfterFen = (CreateFen player endBoard)};
                WhiteMove = None;
                DisplayString = moveNumber.ToString() + ": " + String.Join((if isJump move then "x" else "-"), pdnMove)
            }
        | White ->
            let lastMovePDN = List.last currentGameHistory
            let moveNumber = currentGameHistory.Length
            {
                MoveNumber = moveNumber;
                BlackMove = lastMovePDN.BlackMove;
                WhiteMove = Some { Move = pdnMove; PreviousFen = (CreateFen player startBoard); AfterFen = (CreateFen player endBoard)};
                DisplayString = moveNumber.ToString() + ": " + lastMovePDN.DisplayString + ", " + String.Join((if isJump move then "x" else "-"), pdnMove)
            }

    match player with
    | Black -> currentGameHistory @ [newTurnValue]
    | White -> (List.take (currentGameHistory.Length - 1) currentGameHistory) @ [newTurnValue]

let movePiece startCoord endCoord gameController :Option<GameController> =
    let board = movePiece startCoord endCoord gameController.Board

    match board with
    | None -> None
    | Some b ->
        Some <|
            {
                Board = b;
                CurrentPlayer = match playerTurnEnds [startCoord; endCoord] gameController.Board b with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer        
                CurrentCoord = match playerTurnEnds [startCoord; endCoord] gameController.Board b with
                                | true -> None
                                | false -> Some endCoord
                MoveHistory = getGameHistory gameController.MoveHistory gameController.CurrentPlayer [startCoord; endCoord] gameController.Board b
            }

let move (move :Coord seq) (gameController) :Option<GameController> =
    let board = moveSequence move (Some gameController.Board)
    let moveAsList = (List.ofSeq move)
    match board with
    | None -> None
    | Some b ->
        Some <|
            {
                Board = b;
                CurrentPlayer = match playerTurnEnds moveAsList gameController.Board b with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer        
                CurrentCoord = match playerTurnEnds moveAsList gameController.Board b with
                                | true -> None
                                | false -> Some (Seq.last move)
                MoveHistory = getGameHistory gameController.MoveHistory gameController.CurrentPlayer moveAsList gameController.Board b
            }

let getMove searchDepth gameController =
    (minimax gameController.CurrentPlayer searchDepth None None gameController.Board).Move

let isWon controller =
    isWon controller.Board