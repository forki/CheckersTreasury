namespace Checkers.Extensions

open Checkers
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Types
open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type ExtensionMethods() =

    [<Extension>]
    static member IsValidMove(board :Board, startCoord, endCoord) =
        coordExists startCoord &&
        coordExists endCoord &&
        moveIsDiagonal startCoord endCoord &&
        match Math.Abs(startCoord.Row - endCoord.Row) with
        | 1 -> isValidHop startCoord endCoord board && not <| jumpAvailable (square startCoord board).Value.Player board
        | 2 -> isValidJump startCoord endCoord board
        | _ -> false

    [<Extension>]
    static member Move(board :Board, startCoord :Coord, endCoord :Coord) :Option<Board> =
        match ExtensionMethods.IsValidMove(board, startCoord, endCoord) with
        | false -> None
        | true ->
            match Math.Abs(startCoord.Row - endCoord.Row) with
            | 1 -> Some <| hop startCoord endCoord board
            | 2 -> Some <| jump startCoord endCoord board
            | _ -> None

    [<Extension>]
    static member Move(board :Board, coordinates :IEnumerable<Coord>) =
        let coords = List.ofSeq(coordinates)

        match coords.Length with
        | b when b >= 3 ->
            let newBoard = ExtensionMethods.Move (board, coords.Head, coords.[1])
            ExtensionMethods.Move (newBoard, coords.Tail)
        | _ -> ExtensionMethods.Move (board, coords.Head, coords.[1])

    static member internal Move(board :Option<Board> ,coordinates :IEnumerable<Coord>) =
        match board.IsSome with
        | false -> None
        | true -> ExtensionMethods.Move(board.Value, coordinates)