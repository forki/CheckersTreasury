module Checkers.GameVariant
open Checkers.Generic
open Checkers.Board

type AiMembers =
    {
        uncheckedMoveSequence :Coord seq -> Board -> Board
        calculateMoves :Player -> Board -> Move List
        winningPlayer :Board -> Player Option
        calculateWeightDifference :Board -> float
    }
with
    static member AmericanCheckers =
        {
            uncheckedMoveSequence = Checkers.Variants.AmericanCheckers.uncheckedMoveSequence
            calculateMoves = Checkers.AIs.AmericanCheckersAI.calculateMoves
            winningPlayer = Checkers.Variants.AmericanCheckers.winningPlayer
            calculateWeightDifference = Checkers.AIs.AmericanCheckersAI.calculateWeightDifference
        }
    static member PoolCheckers =
        {
            uncheckedMoveSequence = Checkers.Variants.PoolCheckers.uncheckedMoveSequence
            calculateMoves = Checkers.AIs.PoolCheckersAI.calculateMoves
            winningPlayer = Checkers.Variants.PoolCheckers.winningPlayer
            calculateWeightDifference = Checkers.AIs.PoolCheckersAI.calculateWeightDifference
        }

type PdnMembers =
    {
        pdnBoard :int Option [,]
        pdnBoardCoords :Coord List
    }
with
    static member AmericanCheckers =
        {
            pdnBoard = Checkers.Variants.AmericanCheckers.pdnBoard
            pdnBoardCoords = Checkers.Variants.AmericanCheckers.pdnBoardCoords
        }
    static member PoolCheckers =
        {
            pdnBoard = Checkers.Variants.PoolCheckers.pdnBoard
            pdnBoardCoords = Checkers.Variants.PoolCheckers.pdnBoardCoords
        }

type ApiMembers =
    {
        isValidMove :Coord -> Coord -> Board -> bool
        movePiece :Coord -> Coord -> Board -> Board Option
        moveSequence :Coord seq -> Board Option -> Board Option
        isJump :Move -> bool
        winningPlayer :Board -> Player Option
        playerTurnEnds :Move -> Board -> Board -> bool
    }
with
    static member AmericanCheckers =
        {
            isValidMove = Checkers.Variants.AmericanCheckers.isValidMove
            movePiece = Checkers.Variants.AmericanCheckers.movePiece
            moveSequence = Checkers.Variants.AmericanCheckers.moveSequence
            isJump = Checkers.Variants.AmericanCheckers.isJump
            winningPlayer = Checkers.Variants.AmericanCheckers.winningPlayer
            playerTurnEnds = Checkers.Variants.AmericanCheckers.playerTurnEnds
        }
    static member PoolCheckers =
        {
            isValidMove = Checkers.Variants.PoolCheckers.isValidMove
            movePiece = Checkers.Variants.PoolCheckers.movePiece
            moveSequence = Checkers.Variants.PoolCheckers.moveSequence
            isJump = Checkers.Variants.PoolCheckers.isJump
            winningPlayer = Checkers.Variants.PoolCheckers.winningPlayer
            playerTurnEnds = Checkers.Variants.PoolCheckers.playerTurnEnds
        }

type GameVariant =
    {
        variant :Variant
        aiMembers :AiMembers
        pdnMembers :PdnMembers
        apiMembers :ApiMembers
    }
with
    static member AmericanCheckers =
        {
            variant = AmericanCheckers
            aiMembers = AiMembers.AmericanCheckers
            pdnMembers = PdnMembers.AmericanCheckers
            apiMembers = ApiMembers.AmericanCheckers
        }
    static member PoolCheckers =
        {
            variant = PoolCheckers
            aiMembers = AiMembers.PoolCheckers
            pdnMembers = PdnMembers.PoolCheckers
            apiMembers = ApiMembers.PoolCheckers
        }