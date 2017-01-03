module public Checkers.GameController
open Checkers.Types
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open System
open System.Collections
open System.Linq

[<Literal>]
let BlackSymbol = 'B'

[<Literal>]
let WhiteSymbol = 'W'

type GameController = { Board :Board; CurrentPlayer :Player; CurrentCoord :Option<Coord>; MoveHistory :PDNTurn List }

let internal getPieceNotation (fenSections :string[]) (playerSymbol :char) =
    (match fenSections.[1].[0] with
     | c when c = playerSymbol -> fenSections.[1]
     | _ -> fenSections.[2])
         .Remove(0, 1)
         .Split(',')

let controllerFromFen (fen :string) =
    let board = Board.emptyBoard()

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

        let boardCoord = PDNBoardCoords.[fenNumber]
        board.[boardCoord.Row].[boardCoord.Column] <-
            match (player, isKing) with
            | (White, true) -> Piece.whiteKing
            | (White, false) -> Piece.whiteChecker
            | (Black, true) -> Piece.blackKing
            | (Black, false) -> Piece.blackChecker
        
        if not tail.IsEmpty then loop tail player

    loop (List.ofArray whitePieces) Player.White
    loop (List.ofArray blackPieces) Player.Black

    {Board = (listFromSeq ((Seq.map (fun (row :Generic.List<Option<Piece>>) -> row.AsEnumerable()) board).AsEnumerable())); CurrentPlayer = playerTurn; CurrentCoord = None; MoveHistory = []}

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
                let fenNumber = (square coord PDNBoard).Value
                loop (fenNumbers @ [(if isKing then "K" else "") + fenNumber.ToString()]) player c
            | false -> loop fenNumbers player c
        | None -> fenNumbers

    let whitePieceFEN = String.Join(",", (loop [] Player.White {Row = 0; Column = 0}))
    let blackPieceFEN = String.Join(",", (loop [] Player.Black {Row = 0; Column = 0}))

    "[FEN \"" + turnSymbol.ToString() + ":W" + whitePieceFEN + ":B" + blackPieceFEN + "\"]"

let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }