module GameControllerTests

open Checkers
open Checkers.GameController
open Checkers.Types
open Checkers.FSharpExtensions
open Checkers.PublicAPI
open Xunit

[<Fact>]
let ``Game won returns player``() =
    let board =
        [
            [Piece.whiteKing; None; None; None; None; None; None; None];
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let gameController = { Board = board; Player = Black }

    Assert.Equal(Black, (isWon gameController).Value)

[<Fact>]
let ``Game not won returns None``() =
    let gameController = { Board = Board.defaultBoard; Player = Black }

    Assert.True((isWon gameController).IsNone)

[<Fact>]
let ``Player turned not ended``() =
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

    Assert.False(playerTurnEnds { Row = 4; Column = 4 } { Row = 2; Column = 2 } board)

[<Fact>]
let ``Player turned not ended--checker piece``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackChecker; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.False(playerTurnEnds { Row = 0; Column = 0 } { Row = 2; Column = 2 } board)

[<Fact>]
let ``Player turn ends when move is hop``() =
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

    Assert.True(playerTurnEnds { Row = 3; Column = 3 } { Row = 2; Column = 2 } board)

[<Fact>]
let ``Player turn ends when no jumps available``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(playerTurnEnds { Row = 4; Column = 4 } { Row = 2; Column = 2 } board)