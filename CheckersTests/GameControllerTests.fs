module GameControllerTests

open Checkers
open Checkers.GameController
open Checkers.Types
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

    let gameController = { Board = board; CurrentPlayer = Black; CurrentCoord = None }

    Assert.Equal(Black, (isWon gameController).Value)

[<Fact>]
let ``Game not won returns None``() =
    let gameController = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None }

    Assert.True((isWon gameController).IsNone)

[<Fact>]
let ``Player turn not ended``() =
    let originalBoard =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
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

    Assert.False(playerTurnEnds { Row = 4; Column = 4 } { Row = 2; Column = 2 } originalBoard newBoard)

[<Fact>]
let ``Player turn not ended--checker piece``() =
    let originalBoard =
        [
            [Piece.blackChecker; None; None; None; None; None; None; None];
            [None; Piece.whiteChecker; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
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

    Assert.False(playerTurnEnds { Row = 0; Column = 0 } { Row = 2; Column = 2 } originalBoard newBoard)

[<Fact>]
let ``Player turn ends when move is hop``() =
    let originalBoard =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
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

    Assert.True(playerTurnEnds { Row = 3; Column = 3 } { Row = 2; Column = 2 } originalBoard newBoard)

[<Fact>]
let ``Player turn ends when no jumps available``() =
    let originalBoard =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.whiteChecker; None; None; None; None];
            [None; None; None; None; Piece.blackKing; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
        
    let newBoard =
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

    Assert.True(playerTurnEnds { Row = 4; Column = 4 } { Row = 2; Column = 2 } originalBoard newBoard)

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
let ``Turn cannot continue after promotion``() =
    let originalBoard =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.blackChecker; None; Piece.blackChecker; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let newBoard =
        [
            [None; None; None; Piece.whiteKing; None; None; None; None];
            [None; None; Piece.blackChecker; None; Piece.blackChecker; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    Assert.True(playerTurnEnds { Row = 2; Column = 5 } { Row = 0; Column = 3 } originalBoard newBoard)

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