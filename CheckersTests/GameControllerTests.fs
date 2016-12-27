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

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = None }

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

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = Some { Row = 2; Column = 2 } }

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

    let controller = { Board = board; CurrentPlayer = Black; CurrentCoord = Some { Row = 2; Column = 2 } }

    Assert.True(isValidMove { Row = 2; Column = 2 } { Row = 0; Column = 0 } controller)