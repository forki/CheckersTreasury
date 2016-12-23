module MinimaxTests
open Checkers
open Checkers.Types
open Checkers.Minimax
open System
open Xunit

[<Fact>]
let ``chooseNewAlpha picks lowest value for black``() =
    let newAlpha = chooseNewAlpha Black 1 0
    Assert.Equal(0, newAlpha)

[<Fact>]
let ``chooseNewAlpha picks highest value for white``() =
    let newAlpha = chooseNewAlpha White 1 0
    Assert.Equal(1, newAlpha)

[<Fact>]
let ``chooseNewBeta picks highest value for black``() =
    let newBeta = chooseNewBeta Black 1 0
    Assert.Equal(1, newBeta)

[<Fact>]
let ``chooseNewBeta picks lowest value for white``() =
    let newBeta = chooseNewBeta White 1 0
    Assert.Equal(0, newBeta)

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

    let move = minimax White 3 Double.NegativeInfinity Double.PositiveInfinity board
    Assert.Contains(move.Move.[1], [{Row = 2; Column = 7}; {Row = 2; Column = 5}])

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

    let move = minimax Black 0 Double.NegativeInfinity Double.PositiveInfinity board
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

    let move = minimax White 1 Double.NegativeInfinity Double.PositiveInfinity board
    Assert.Equal({Row = 3; Column = 6}, move.Move.[1])