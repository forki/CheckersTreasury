module internal Checkers.FSharpExtensions
open Checkers
open Checkers.Types
open System

let internal getJumpedCoord startCoord endCoord =
    { Row = startCoord.Row - Math.Sign(startCoord.Row - endCoord.Row); Column = startCoord.Column - Math.Sign(startCoord.Column - endCoord.Column) }

let internal moveIsDiagonal startCoord endCoord =
    startCoord <> endCoord &&
    abs (startCoord.Row - endCoord.Row) = abs (startCoord.Column - endCoord.Column)

let internal otherPlayer player =
    match player with
    | White -> Black
    | Black -> White