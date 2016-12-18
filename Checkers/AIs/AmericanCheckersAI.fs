module Checkers.AmericanCheckersAI

open Checkers.Board
open Checkers.Variants.AmericanCheckers
open Checkers.Types
open System

let checkerWeights =
    [[1.20; 1.20; 1.20; 1.10];
    [1.15; 1.05; 1.0; 1.10];
    [1.10; 1.0; 1.05; 1.15];
    [1.15; 1.05; 1.0; 1.10];
    [1.10; 1.0; 1.05; 1.15];
    [1.15; 1.05; 1.0; 1.10];
    [1.10; 1.0; 1.05; 1.15];
    [1.10; 1.20; 1.20; 1.20]]

let kingWeights =
    [[1.05; 1.0; 1.0; 1.0];
    [1.05; 1.10; 1.05; 1.05];
    [1.10; 1.15; 1.10; 1.0];
    [1.0; 1.15; 1.20; 1.05];
    [1.05; 1.20; 1.15; 1.0];
    [1.0; 1.10; 1.15; 1.10];
    [1.05; 1.05; 1.10; 1.05];
    [1.0; 1.0; 1.0; 1.05]]

let calculateCheckerWeight coord (board :Board) =
    let piece = square coord board
    let kingRow = kingRowIndex piece.Value.Player
    8.0 - (float <| Math.Abs(kingRow - coord.Row)) + (square coord checkerWeights)

let calculateKingWeight coord =
    10.0 + (square coord kingWeights)

let calculateWeight coord (board :Board) =
    let piece = square coord board
    match piece.Value.PieceType with
    | Checker -> calculateCheckerWeight coord board
    | King -> calculateKingWeight coord