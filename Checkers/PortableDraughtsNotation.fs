module Checkers.PortableDraughtsNotation
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.GameController
open System

[<Literal>]
let BlackSymbol = 'B'

[<Literal>]
let WhiteSymbol = 'W'

let internal getPieceNotation (fenSections :string[]) (playerSymbol :char) =
    (match fenSections.[1].[0] with
     | c when c = playerSymbol -> fenSections.[1]
     | _ -> fenSections.[2])
         .Remove(0, 1)
         .Split([|','|], StringSplitOptions.RemoveEmptyEntries)

let rec addPieces (fenPieces :string List) player (board :Board) (pdnBoardCoords :Coord List) =
    let (head::tail) = fenPieces
    let isKing = head.[0] = 'K'
    let fenNumber =
        match isKing with
        | true -> Int32.Parse(head.Remove(0, 1))
        | false -> Int32.Parse(head)

    let boardCoord = pdnBoardCoords.[fenNumber]
    board.[boardCoord.Row, boardCoord.Column] <-
        match (player, isKing) with
        | (White, true) -> Piece.whiteKing
        | (White, false) -> Piece.whiteChecker
        | (Black, true) -> Piece.blackKing
        | (Black, false) -> Piece.blackChecker
    
    if not tail.IsEmpty then addPieces tail player board pdnBoardCoords

let controllerFromFen (fen :string) (pdnBoardCoords :Coord List) =
    let fenValue = fen.Split('"').[1]
    let fenSubsections = fenValue.Split(':')
    let playerTurn =
        match fenSubsections.[0].[0] with
        | BlackSymbol -> Player.Black
        | WhiteSymbol -> Player.White
        
    let whitePieces = getPieceNotation fenSubsections WhiteSymbol
    let blackPieces = getPieceNotation fenSubsections BlackSymbol
    
    let board = Board.emptyBoardList()
    if whitePieces.Length > 0 then addPieces (List.ofArray whitePieces) Player.White board pdnBoardCoords
    if blackPieces.Length > 0 then addPieces (List.ofArray blackPieces) Player.Black board pdnBoardCoords

    {
        Board = board;
        CurrentPlayer = playerTurn;
        InitialPosition = fen;
        MoveHistory =
            match playerTurn with
            | White -> [{MoveNumber = 1; BlackMove = {Move = []; ResultingFen = fen; DisplayString = "…"}; WhiteMove = None}]
            | Black -> []
        CurrentCoord = None;
    }

let createFen player (board :Board) (pdnBoard :int Option [,]) =
    let turnSymbol =
        match player with
        | White -> WhiteSymbol
        | Black -> BlackSymbol

    let rec loop (fenNumbers :string List) player coord =
        match nextPoint coord 7 7 with
        | Some c ->
            let piece = square coord board
            match piece.IsSome && isPlayerPiece player coord board with
            | true ->
                let isKing = piece.Value.PieceType = PieceType.King
                let fenNumber = (square coord pdnBoard).Value
                loop (fenNumbers @ [(if isKing then "K" else "") + fenNumber.ToString()]) player c
            | false -> loop fenNumbers player c
        | None -> fenNumbers

    let whitePieceFEN = String.Join(",", (loop [] Player.White {Row = 0; Column = 0}))
    let blackPieceFEN = String.Join(",", (loop [] Player.Black {Row = 0; Column = 0}))

    "[FEN \"" + turnSymbol.ToString() + ":W" + whitePieceFEN + ":B" + blackPieceFEN + "\"]"