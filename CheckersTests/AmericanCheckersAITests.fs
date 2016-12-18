module AmericanCheckersAITests
open Checkers
open Checkers.Types
open Checkers.AIs.AmericanCheckersAI
open Xunit

[<Fact>]
let ``Calculate moves returns correct number of hops``() =
    let moves = calculateMoves Black Board.defaultBoard
    Assert.Equal(7, moves.Length)

[<Fact>]
let ``Calculate moves returns correct number of jumps``() =
    let board =
        [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(1, moves.Length)

[<Fact>]
let ``Calculate moves prefers jumps to hops``() =
    let board =
        [
            [None; None; Piece.blackChecker; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let moves = calculateMoves Black board
    Assert.Equal(1, moves.Length)