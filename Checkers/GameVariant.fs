﻿module Checkers.GameVariant
open Checkers.Generic
open Checkers.Board

type GameVariant =
    {
        winningPlayer :Board -> Player Option
        calculateWeightDifference :Board -> float
        uncheckedMoveSequence :Coord seq -> Board -> Board
        calculateMoves :Player -> Board -> Move List
    }
with
    static member AmericanCheckers =
        {
            winningPlayer = Checkers.Variants.AmericanCheckers.winningPlayer
            calculateWeightDifference = Checkers.AIs.AmericanCheckersAI.calculateWeightDifference
            uncheckedMoveSequence = Checkers.Variants.AmericanCheckers.uncheckedMoveSequence
            calculateMoves = Checkers.AIs.AmericanCheckersAI.calculateMoves
        }