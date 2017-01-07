module Checkers.PortableDraughtsNotation
open Checkers.Generic
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
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

let controllerFromFen (fen :string) =
    let board = Board.emptyBoardList()

    let fenValue = fen.Split('"').[1]
    let fenSubsections = fenValue.Split(':')
    let playerTurn =
        match fenSubsections.[0].[0] with
        | BlackSymbol -> Player.Black
        | WhiteSymbol -> Player.White
        
    let whitePieces = getPieceNotation fenSubsections WhiteSymbol
    let blackPieces = getPieceNotation fenSubsections BlackSymbol

    let rec loop (fenPieces :string List) player =
        let (head::tail) = fenPieces
        let isKing = head.[0] = 'K'
        let fenNumber =
            match isKing with
            | true -> System.Int32.Parse(head.Remove(0, 1))
            | false -> System.Int32.Parse(head)

        let boardCoord = pdnBoardCoords.[fenNumber]
        board.[boardCoord.Row, boardCoord.Column] <-
            match (player, isKing) with
            | (White, true) -> Piece.whiteKing
            | (White, false) -> Piece.whiteChecker
            | (Black, true) -> Piece.blackKing
            | (Black, false) -> Piece.blackChecker
        
        if not tail.IsEmpty then loop tail player

    if whitePieces.Length > 0 then loop (List.ofArray whitePieces) Player.White
    if blackPieces.Length > 0 then loop (List.ofArray blackPieces) Player.Black

    {
        Board = board;
        CurrentPlayer = playerTurn;
        InitialPosition = fen;
        MoveHistory = [];
        CurrentCoord = None;
    }

let createFen player (board :Board) =
    let turnSymbol =
        match player with
        | White -> WhiteSymbol
        | Black -> BlackSymbol

    let rec loop (fenNumbers :string List) player coord =
        match nextPoint coord with
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