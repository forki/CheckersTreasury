﻿module public Checkers.PublicAPI
open Checkers.Generic
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants
open Checkers.GameController
open Checkers.PortableDraughtsNotation
open Checkers.GameVariant
open Checkers.Minimax
open System

let getGameVariant variant =
    match variant with
    | AmericanCheckers -> GameVariant.AmericanCheckers
    | PoolCheckers -> GameVariant.PoolCheckers

let pdnBoard variant =
    variant.pdnBoard

let pdnBoardCoords variant =
    variant.pdnBoardCoords

let isValidMove startCoord endCoord gameController =
    gameController.Variant.apiMembers.isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let internal getDisplayString variant (pdnTurn :int List) (move :Move) =
    String.Join((if variant.isJump move then "x" else "-"), pdnTurn)
    
let internal getPdnForMove gameController move boardFen =
    let gameHistory = gameController.MoveHistory
    let pdnMove = (List.map (fun item -> (square item gameController.Variant.pdnMembers.pdnBoard).Value) move)

    let moveNumber =
        match gameController.CurrentPlayer with
        | Black -> gameHistory.Length + 1
        | White -> gameHistory.Length

    let blackMove =
        match gameController.CurrentPlayer with
        | Black -> { Move = pdnMove; ResultingFen = boardFen; DisplayString = getDisplayString gameController.Variant.apiMembers pdnMove move }
        | White -> (List.last gameHistory).BlackMove

    let whiteMove =
        match gameController.CurrentPlayer with
        | Black -> None
        | White -> Some { Move = pdnMove; ResultingFen = boardFen; DisplayString = getDisplayString gameController.Variant.apiMembers pdnMove move }

    {MoveNumber = moveNumber; BlackMove = blackMove; WhiteMove = whiteMove}
    
let internal getPdnForContinuedMove gameController move boardFen =
    let gameHistory = gameController.MoveHistory
    
    let lastMovePdn = List.last gameHistory
    let pdnMove = (List.map (fun item -> (square item gameController.Variant.pdnMembers.pdnBoard).Value) move)

    let moveNumber = lastMovePdn.MoveNumber

    let blackMove =
        match gameController.CurrentPlayer with
        | Black ->
            let newPdnMove = lastMovePdn.BlackMove.Move @ pdnMove.Tail
            { Move = newPdnMove; ResultingFen = boardFen; DisplayString = getDisplayString gameController.Variant.apiMembers newPdnMove move }
        | White -> lastMovePdn.BlackMove

    let whiteMove =
        match gameController.CurrentPlayer with
        | Black -> None
        | White ->
            let newPdnMove = lastMovePdn.WhiteMove.Value.Move @ pdnMove.Tail
            Some { Move = newPdnMove; ResultingFen = boardFen; DisplayString = getDisplayString gameController.Variant.apiMembers newPdnMove move }

    {MoveNumber = moveNumber; BlackMove = blackMove; WhiteMove = whiteMove}

let internal getGameHistory gameController move boardFen =
    let isContinuedMove = gameController.CurrentCoord <> None

    let newTurnValue =
        match isContinuedMove with
        | false -> getPdnForMove gameController move boardFen
        | true -> getPdnForContinuedMove gameController move boardFen

    match gameController.CurrentPlayer, isContinuedMove with
    | Black, false -> gameController.MoveHistory @ [newTurnValue]
    | _ ->
        match gameController.MoveHistory with
        | [] -> [newTurnValue]
        | _ -> (List.take (gameController.MoveHistory.Length - 1) gameController.MoveHistory) @ [newTurnValue]

let movePiece startCoord endCoord gameController :Option<GameController> =
    let board = gameController.Variant.apiMembers.movePiece startCoord endCoord gameController.Board

    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = gameController.Variant.apiMembers.playerTurnEnds [startCoord; endCoord] gameController.Board b
        let nextPlayerTurn = 
            match gameController.Variant.apiMembers.playerTurnEnds [startCoord; endCoord] gameController.Board b with
            | true -> otherPlayer gameController.CurrentPlayer
            | false -> gameController.CurrentPlayer

        Some <|
            {
                Variant = gameController.Variant
                Board = b
                CurrentPlayer = nextPlayerTurn
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController [startCoord; endCoord] (createFen gameController.Variant.pdnMembers nextPlayerTurn b)
                CurrentCoord = if isTurnEnding then None else Some endCoord
            }

let move (move :Coord seq) (gameController) :Option<GameController> =
    let board = gameController.Variant.apiMembers.moveSequence move (Some gameController.Board)
    let moveAsList = (List.ofSeq move)

    match board with
    | None -> None
    | Some b ->
        let isTurnEnding = gameController.Variant.apiMembers.playerTurnEnds moveAsList gameController.Board b
        let nextPlayerTurn =
            match gameController.Variant.apiMembers.playerTurnEnds moveAsList gameController.Board b with
            | true -> otherPlayer gameController.CurrentPlayer
            | false -> gameController.CurrentPlayer

        Some <|
            {
                Variant = gameController.Variant
                Board = b;
                CurrentPlayer = nextPlayerTurn
                InitialPosition = gameController.InitialPosition
                MoveHistory = getGameHistory gameController moveAsList (createFen gameController.Variant.pdnMembers nextPlayerTurn b)
                CurrentCoord = if isTurnEnding then None else Some (Seq.last move)
            }

let getMove searchDepth gameController =
    (minimax gameController.CurrentPlayer searchDepth searchDepth None None gameController.Board gameController.Variant).Move

let takeBackMove gameController =
    let fen =
        match gameController.CurrentPlayer, gameController.MoveHistory.Length with
        | Black, l when l >= 1 -> (List.last gameController.MoveHistory).BlackMove.ResultingFen
        | White, l when l >= 2 ->(gameController.MoveHistory.[gameController.MoveHistory.Length - 2]).WhiteMove.Value.ResultingFen
        | _ -> gameController.InitialPosition

    let newMoveHistory =
        match gameController.CurrentPlayer, gameController.MoveHistory with
        | White, _ -> List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory
        | Black, [] -> []
        | Black, _ ->
            let lastMove = (List.last gameController.MoveHistory)
            let newLastMove = {lastMove with WhiteMove = None}
            List.truncate (gameController.MoveHistory.Length - 1) gameController.MoveHistory @ [newLastMove]
    
    {(controllerFromFen gameController.Variant fen) with MoveHistory = newMoveHistory}

let winningPlayer gameController =
    gameController.Variant.apiMembers.winningPlayer gameController.Board

let isWon controller =
    let player = winningPlayer controller
    player.IsSome &&
    player.Value <> controller.CurrentPlayer

let createFen variant player (board :Board) =
    createFen variant player board

let controllerFromFen variant fen =
    controllerFromFen variant fen