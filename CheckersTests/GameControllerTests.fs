﻿module GameControllerTests
open Checkers
open Checkers.GameController
open Checkers.Types
open Checkers.PublicAPI
open Xunit

[<Fact>]
let ``Opponent cannot move when player turn not ended``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }

    Assert.False(isValidMove { Row = 1; Column = 1 } { Row = 3; Column = 3 } controller)

[<Fact>]
let ``Player cannot move two pieces in multijump``() =
    let board =
        [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = Some { Row = 2; Column = 2 }; MoveHistory = [] }

    Assert.False(isValidMove { Row = 0; Column = 2 } { Row = 2; Column = 0 } controller)

[<Fact>]
let ``Player can move one piece in multijump``() =
    let board =
        [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = Some { Row = 2; Column = 2 }; MoveHistory = [] }

    Assert.True(isValidMove { Row = 2; Column = 2 } { Row = 0; Column = 0 } controller)

[<Fact>]
let ``Moving records move history: black hop``() =
    let controller = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }
    let newController = movePiece { Row = 2; Column = 1 } { Row = 3; Column = 0 } controller

    Assert.Equal("1: 9-13", (List.last newController.Value.MoveHistory).DisplayString)

[<Fact>]
let ``Moving records move history: black jump``() =
    let board =
        [
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }
    let newController = movePiece { Row = 0; Column = 1 } { Row = 2; Column = 3 } controller

    Assert.Equal("1: 1x10", (List.last newController.Value.MoveHistory).DisplayString)

[<Fact>]
let ``Moving records move history: white hop``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteKing; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = White; CurrentCoord = None; MoveHistory = [{MoveNumber = 1; BlackMove = [1; 10]; WhiteMove = None; DisplayString = "1x10"}] }
    let newController = movePiece { Row = 3; Column = 4 } { Row = 1; Column = 2 } controller

    Assert.Equal("1: 1x10, 15x6", (List.last newController.Value.MoveHistory).DisplayString)