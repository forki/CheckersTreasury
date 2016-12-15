namespace Checkers.Extensions

open Checkers
open Checkers.FSharpExtensions
open System
open System.Collections.Generic
open System.Runtime.CompilerServices

[<Extension>]
type ExtensionMethods() =

    [<Extension>]
    static member IsValidMove(board :Board, startCoord :Coord, endCoord :Coord) =
        board.CoordExists(startCoord) &&
        board.CoordExists(endCoord) &&
        board.[startCoord].IsSome &&
        moveIsDiagonal(startCoord, endCoord) &&
        match Math.Abs(startCoord.Row - endCoord.Row) with
        | 1 -> board.IsValidHop(startCoord, endCoord)
        | 2 -> board.IsValidJump(startCoord, endCoord)
        | _ -> false

    [<Extension>]
    static member Move(board :Board, startCoord :Coord, endCoord :Coord) :Option<Board> =
        match ExtensionMethods.IsValidMove(board, startCoord, endCoord) with
        | false -> None
        | true ->
            match Math.Abs(startCoord.Row - endCoord.Row) with
            | 1 -> Some <| board.Hop (startCoord, endCoord)
            | 2 -> Some <| board.Jump (startCoord, endCoord)
            | _ -> None

    [<Extension>]
    static member Move(board :Board, coordinates :IEnumerable<Coord>) =
        let coords = List.ofSeq(coordinates)

        match coords.Length with
        | b when b >= 3 ->
            let newBoard = ExtensionMethods.Move (board, coords.Head, coords.[1])
            ExtensionMethods.Move (newBoard, coords.Tail)
        | _ -> ExtensionMethods.Move (board, coords.Head, coords.[1])

    static member internal Move(board :Option<Board>, coordinates :IEnumerable<Coord>) =
        match board.IsSome with
        | false -> None
        | true -> ExtensionMethods.Move(board.Value, coordinates)