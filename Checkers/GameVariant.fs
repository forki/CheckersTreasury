module Checkers.GameVariant
open Checkers.Generic
open Checkers.Board

type GameVariant =
    {
        winningPlayer :Board -> Player Option
        calculateWeightDifference :Board -> float
        isValidMove :Coord -> Coord -> Board -> bool
        movePiece :Coord -> Coord -> Board -> Board Option
        moveSequence :Coord seq -> Board Option -> Board Option
        uncheckedMoveSequence :Coord seq -> Board -> Board
        calculateMoves :Player -> Board -> Move List
        playerTurnEnds :Move -> Board -> Board -> bool
        pdnBoard :int Option [,]
        pdnBoardCoords :Coord List
    }
with
    static member AmericanCheckers =
        {
            winningPlayer = Checkers.Variants.AmericanCheckers.winningPlayer
            calculateWeightDifference = Checkers.AIs.AmericanCheckersAI.calculateWeightDifference
            isValidMove = Checkers.Variants.AmericanCheckers.isValidMove
            movePiece = Checkers.Variants.AmericanCheckers.movePiece
            moveSequence = Checkers.Variants.AmericanCheckers.moveSequence
            uncheckedMoveSequence = Checkers.Variants.AmericanCheckers.uncheckedMoveSequence
            calculateMoves = Checkers.AIs.AmericanCheckersAI.calculateMoves
            playerTurnEnds = Checkers.Variants.AmericanCheckers.playerTurnEnds
            pdnBoard = Checkers.Variants.AmericanCheckers.pdnBoard
            pdnBoardCoords = Checkers.Variants.AmericanCheckers.pdnBoardCoords
        }
    static member PoolCheckers =
        {
            winningPlayer = Checkers.Variants.PoolCheckers.winningPlayer
            calculateWeightDifference = Checkers.AIs.PoolCheckersAI.calculateWeightDifference
            isValidMove = Checkers.Variants.PoolCheckers.isValidMove
            movePiece = Checkers.Variants.PoolCheckers.movePiece
            moveSequence = Checkers.Variants.PoolCheckers.moveSequence
            uncheckedMoveSequence = Checkers.Variants.PoolCheckers.uncheckedMoveSequence
            calculateMoves = Checkers.AIs.PoolCheckersAI.calculateMoves
            playerTurnEnds = Checkers.Variants.PoolCheckers.playerTurnEnds
            pdnBoard = Checkers.Variants.PoolCheckers.pdnBoard
            pdnBoardCoords = Checkers.Variants.PoolCheckers.pdnBoardCoords
        }