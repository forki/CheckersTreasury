module public Checkers.GameController
open Checkers.Types
open Checkers.Piece
open Checkers.Board
open Checkers.Variants.AmericanCheckers
open System.Collections
open System.Linq

type GameController = { Board :Board; CurrentPlayer :Player; CurrentCoord :Option<Coord>; MoveHistory :PDNTurn List }

let internal getPieceNotation (fenSections :string[]) (playerSymbol :char) =
    (match fenSections.[1].[0] with
     | c when c = playerSymbol -> fenSections.[1]
     | _ -> fenSections.[2])
         .Remove(0, 1)
         .Split(',')

let controllerFromFEN (fen :string) =
    let board = Board.emptyBoard

    let fenValue = fen.Split('"').[1]
    let fenSubsections = fenValue.Split(':')
    let playerTurn =
        match fenSubsections.[0] with
        | "B" -> Player.Black
        | "W" -> Player.White
        
    let whitePieces = getPieceNotation fenSubsections 'W'
    let blackPieces = getPieceNotation fenSubsections 'B'

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

let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }