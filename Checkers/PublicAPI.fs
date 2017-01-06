module public Checkers.PublicAPI
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.Minimax
open Checkers.GameController
open Checkers.PortableDraughtsNotation
open System

let isValidMove startCoord endCoord gameController =
    isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let internal getDisplayString (pdnTurn :int List) (move :Move) =
    String.Join((if isJump move then "x" else "-"), pdnTurn)

let internal getGameHistory gameController isMoveEnding move board =
    let gameHistory = gameController.MoveHistory
    let currentPlayer = gameController.CurrentPlayer
    let isContinuedMove = gameController.CurrentCoord <> None

    let pdnMove = (List.map (fun item -> (square item PDNBoard).Value) move)
    let nextPlayer = if isMoveEnding then otherPlayer currentPlayer else currentPlayer

    let newTurnValue =
        match currentPlayer, isContinuedMove with
        | Black, false ->
            let moveNumber = gameHistory.Length + 1
            {
                MoveNumber = moveNumber;
                BlackMove = { Move = pdnMove; ResultingFen = (createFen nextPlayer board); DisplayString = getDisplayString pdnMove move };
                WhiteMove = None;
            }
        | White, false ->
            let lastMovePDN =
                match gameHistory with
                | [] ->
                    {
                        MoveNumber = 0;
                        BlackMove = {Move = []; ResultingFen = gameController.InitialPosition; DisplayString = "…"}
                        WhiteMove = None
                    }
                | _ -> (List.last gameHistory)

            let moveNumber = gameHistory.Length
            {
                MoveNumber = moveNumber;
                BlackMove = lastMovePDN.BlackMove;
                WhiteMove = Some { Move = pdnMove; ResultingFen = (createFen nextPlayer board); DisplayString = getDisplayString pdnMove move };
            }
        | Black, true ->
            let lastMovePDN = List.last gameHistory
            let newPDNMove = lastMovePDN.BlackMove.Move @ pdnMove.Tail
            {
                MoveNumber = lastMovePDN.MoveNumber;
                BlackMove = { Move = newPDNMove; ResultingFen = (createFen nextPlayer board); DisplayString = getDisplayString newPDNMove move };
                WhiteMove = None;
            }
        | White, true ->
            let lastMovePDN = List.last gameHistory
            let newPDNMove = lastMovePDN.WhiteMove.Value.Move @ pdnMove.Tail
            {
                MoveNumber = lastMovePDN.MoveNumber;
                BlackMove = lastMovePDN.BlackMove;
                WhiteMove = Some { Move = newPDNMove; ResultingFen = (createFen nextPlayer board); DisplayString = getDisplayString newPDNMove move };
            }

    match currentPlayer, isContinuedMove with
    | Black, false -> gameHistory @ [newTurnValue]
    | _ ->
        match gameHistory with
        | [] -> [newTurnValue]
        | _ -> (List.take (gameHistory.Length - 1) gameHistory) @ [newTurnValue]

let movePiece startCoord endCoord gameController :Option<GameController> =
    let board = movePiece startCoord endCoord gameController.Board

    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = playerTurnEnds [startCoord; endCoord] gameController.Board b

        Some <|
            {
                Board = b
                CurrentPlayer = match playerTurnEnds [startCoord; endCoord] gameController.Board b with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController isTurnEnding [startCoord; endCoord] b
                CurrentCoord = if isTurnEnding then None else Some endCoord
            }

let move (move :Coord seq) (gameController) :Option<GameController> =
    let board = moveSequence move (Some gameController.Board)
    let moveAsList = (List.ofSeq move)
    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = playerTurnEnds moveAsList gameController.Board b

        Some <|
            {
                Board = b;
                CurrentPlayer = match playerTurnEnds moveAsList gameController.Board b with
                                | true -> otherPlayer gameController.CurrentPlayer
                                | false -> gameController.CurrentPlayer
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController isTurnEnding moveAsList b
                CurrentCoord = if isTurnEnding then None else Some (Seq.last move)
            }

let getMove searchDepth gameController =
    (minimax gameController.CurrentPlayer searchDepth searchDepth None None gameController.Board).Move

let takeBackMove gameController =
    let fen =
        match gameController.CurrentPlayer, gameController.MoveHistory.Length with
        | Black, l when l >= 1 -> (List.last gameController.MoveHistory).BlackMove.ResultingFen
        | White, l when l >= 2 ->(gameController.MoveHistory.[gameController.MoveHistory.Length - 2]).WhiteMove.Value.ResultingFen
        | _ -> gameController.InitialPosition

    let newMoveHistory =
        match gameController.CurrentPlayer with
        | White -> List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory
        | Black ->
            match gameController.MoveHistory with
            | [] -> []
            | _ ->
                let lastMove = (List.last gameController.MoveHistory)
                let newLastMove = {lastMove with WhiteMove = None}
                List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory @ [newLastMove]
    
    {(controllerFromFen fen) with MoveHistory = newMoveHistory}

let isWon controller =
    isWon controller.Board

let getPdnCoord pdnNumber =
    PDNBoardCoords.[pdnNumber]