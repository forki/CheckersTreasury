module MinimaxTests
open Checkers
open Checkers.Types
open Checkers.Minimax
open System
open Xunit

[<Fact>]
let ``chooseNewAlpha picks highest value``() =
    let newAlpha = chooseNewAlpha (Some 1.0) (Some 0.0)
    Assert.Equal(1.0, newAlpha.Value)

[<Fact>]
let ``chooseNewAlpha picks some value``() =
    let newAlpha = chooseNewAlpha None (Some 1.0)
    Assert.Equal(1.0, newAlpha.Value)

[<Fact>]
let ``chooseNewBeta picks lowest value``() =
    let newBeta = chooseNewBeta (Some 1.0) (Some 0.0)
    Assert.Equal(0.0, newBeta.Value)

[<Fact>]
let ``chooseNewBeta picks some value``() =
    let newBeta = chooseNewBeta None (Some 1.0)
    Assert.Equal(1.0, newBeta.Value)

[<Fact>]
let ``AI forces win``() =
    let board =
        [
            [None; None; None; None; None; None; None; Piece.blackKing];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; Piece.whiteKing; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let move = minimax White 3 None None board
    Assert.Equal(2, move.Move.[1].Row)

[<Fact>]
let ``AI prefers double jump to single jump``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; Piece.whiteKing; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; Piece.whiteKing; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let move = minimax Black 1 None None board
    Assert.Equal(3, move.Move.Length)

[<Fact>]
let ``AI should not give free double jump``() =
    let board =
        [
            [None; None; None; None; None; None; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; None; None];
            [None; Piece.blackChecker; None; Piece.blackChecker; None; None; None; Piece.blackChecker];
            [Piece.blackChecker; None; Piece.blackChecker; None; Piece.blackChecker; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; Piece.whiteChecker];
            [Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None];
            [None; Piece.whiteChecker; None; Piece.whiteChecker; None; Piece.whiteChecker; None; None];
            [Piece.whiteChecker; None; None; None; None; None; None; None];
        ];

    let move = minimax White 2 None None board
    Assert.Equal({Row = 3; Column = 6}, move.Move.[1])