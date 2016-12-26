module internal Checkers.Variants.AmericanCheckers
open Checkers.Types
open Checkers.Piece
open Checkers.Board
open Checkers.FSharpExtensions
open System
open System.Collections.Generic

[<Literal>]
let Rows = 7
    
[<Literal>]
let Columns = 7

let internal kingRowIndex(player) =
    match player with
    | Player.Black -> Rows
    | Player.White -> 0
    
let internal coordExists coord =
    coord.Row >= 0 && coord.Row <= Rows &&
    coord.Column >= 0 && coord.Column <= Columns

let internal checkMoveDirection piece startCoord endCoord =
    match piece.PieceType with
    | PieceType.Checker ->
        match piece.Player with
        | Player.Black -> startCoord.Row < endCoord.Row
        | Player.White -> startCoord.Row > endCoord.Row
    | PieceType.King -> true

let internal isValidCheckerHop startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value

    checkMoveDirection piece startCoord endCoord &&
    (square endCoord board).IsNone

let internal isValidKingHop endCoord (board :Board) =
    (square endCoord board).IsNone

let internal isValidCheckerJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value
        
    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board
        
    checkMoveDirection piece startCoord endCoord &&
    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player

let internal isValidKingJump startCoord endCoord (board :Board) =
    let piece = (square startCoord board).Value

    let jumpedCoord = getJumpedCoord startCoord endCoord
    let jumpedPiece = square jumpedCoord board

    (square endCoord board).IsNone &&
    jumpedPiece.IsSome &&
    jumpedPiece.Value.Player <> piece.Player
            
let internal isValidHop startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | PieceType.Checker -> isValidCheckerHop startCoord endCoord board
    | PieceType.King -> isValidKingHop endCoord board
        
let internal isValidJump startCoord endCoord (board :Board) =
    match (square startCoord board).Value.PieceType with
    | PieceType.Checker -> isValidCheckerJump startCoord endCoord board
    | PieceType.King -> isValidKingJump startCoord endCoord board

let internal hasValidHop startCoord (board :Board) =
    let hopCoords =
        [
            offset startCoord {Row = -1; Column = 1};
            offset startCoord {Row = -1; Column = -1};
            offset startCoord {Row = 1; Column = 1};
            offset startCoord {Row = 1; Column = -1}
        ]

    let flattenedList = seq {
        for coord in hopCoords do
        yield coordExists coord && isValidHop startCoord coord board }

    flattenedList |> Seq.exists id

let internal hasValidJump startCoord (board :Board) =
    let jumpCoords =
        [
            offset startCoord {Row = -2; Column = 2};
            offset startCoord {Row = -2; Column = -2};
            offset startCoord {Row = 2; Column = 2};
            offset startCoord {Row = 2; Column = -2}
        ]

    let flattenedList = seq {
        for coord in jumpCoords do
        yield coordExists coord && isValidJump startCoord coord board }
            
    flattenedList |> Seq.exists id

let internal jumpAvailable player (board :Board) =
    let pieceHasJump row column =
        let piece = board.[row].[column]
        piece.IsSome && piece.Value.Player = player && hasValidJump { Row = row; Column = column } board

    let flattenedList = seq {
        for row in 0 .. Rows do
        for column in 0 .. Columns do
        yield (pieceHasJump row column) }
            
    flattenedList |> Seq.exists id

let internal moveAvailable (board :Board) player =
    let pieceHasMove row column =
        let piece = board.[row].[column]
        piece.IsSome &&
        piece.Value.Player = player &&
        (hasValidJump { Row = row; Column = column } board || hasValidHop { Row = row; Column = column } board)

    let flattenedList = seq {
        for row in 0 .. Rows do
        for column in 0 .. Columns do
        yield (pieceHasMove row column) }
            
    flattenedList |> Seq.exists id

let isWon (board :Board) =
    match (moveAvailable board) with
    | x when not <| x White -> Some Black
    | x when not <| x Black -> Some White
    | _ -> None

let internal setPieceAt coord piece (board :Board) =
    let boardItems = List.init (Rows + 1) (fun row ->
        match row with
        | i when i = coord.Row ->
            List.init (Columns + 1) (fun col ->
                match col with
                | j when j = coord.Column -> piece
                | _ -> board.[row].[col]
            )
        | _ -> board.[row]
    )

    boardItems

let internal jump startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex((square startCoord board).Value.Player)

    let piece =
        match endCoord.Row with
        | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
        | _ -> (square startCoord board)

    let jumpedCoord = getJumpedCoord startCoord endCoord

    board
    |> setPieceAt startCoord None
    |> setPieceAt endCoord piece
    |> setPieceAt jumpedCoord None

let internal hop startCoord endCoord (board :Board) =
    let kingRowIndex = kingRowIndex (square startCoord board).Value.Player

    let piece =
        match endCoord.Row with
        | row when row = kingRowIndex -> Some <| Promote (square startCoord board).Value
        | _ -> (square startCoord board)
            
    board
    |> setPieceAt startCoord None
    |> setPieceAt endCoord piece

let internal playerTurnEnds lastMoveStartCoord lastMoveEndCoord (originalBoard :Board) (currentBoard :Board) =
    let lastMoveWasJump = Math.Abs(lastMoveStartCoord.Row - lastMoveEndCoord.Row) = 2
    let pieceWasPromoted = (square lastMoveEndCoord currentBoard).Value.PieceType = King &&
                            (square lastMoveStartCoord originalBoard).Value.PieceType = Checker

    pieceWasPromoted ||
    not (lastMoveWasJump && hasValidJump lastMoveEndCoord currentBoard)

let public isValidMove startCoord endCoord (board :Board) =
    coordExists startCoord &&
    coordExists endCoord &&
    moveIsDiagonal startCoord endCoord &&
    (square startCoord board).IsSome &&
    match Math.Abs(startCoord.Row - endCoord.Row) with
    | 1 -> isValidHop startCoord endCoord board && not <| jumpAvailable (square startCoord board).Value.Player board
    | 2 -> isValidJump startCoord endCoord board
    | _ -> false

let public movePiece startCoord endCoord (board :Board) :Option<Board> =
    match isValidMove startCoord endCoord board with
    | false -> None
    | true ->
        match Math.Abs(startCoord.Row - endCoord.Row) with
        | 1 -> Some <| hop startCoord endCoord board
        | 2 -> Some <| jump startCoord endCoord board
        | _ -> None

let rec public moveSequence (coordinates :Coord seq) (board :Option<Board>) =
    let coords = List.ofSeq(coordinates)

    match board.IsNone with
    | true -> None
    | false ->
        match coords.Length with
        | b when b >= 3 ->
            let newBoard = movePiece coords.Head coords.[1] board.Value
            moveSequence coords.Tail newBoard
        | _ -> movePiece coords.Head coords.[1] board.Value

let internal uncheckedMovePiece startCoord endCoord (board :Board) =
    match Math.Abs(startCoord.Row - endCoord.Row) with
    | 1 -> hop startCoord endCoord board
    | 2 -> jump startCoord endCoord board

let rec internal uncheckedMoveSequence (coordinates :Coord seq) (board :Board) =
    let coords = List.ofSeq(coordinates)

    match coords.Length with
    | b when b >= 3 ->
        let newBoard = uncheckedMovePiece coords.Head coords.[1] board
        uncheckedMoveSequence coords.Tail newBoard
    | _ -> uncheckedMovePiece coords.Head coords.[1] board