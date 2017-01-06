﻿module GameControllerTests
open Checkers
open Checkers.GameController
open Checkers.PortableDraughtsNotation
open Checkers.Types
open Checkers.PublicAPI
open Xunit

[<Fact>]
let ``Opponent cannot move when player turn not ended``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; InitialPosition = ""; MoveHistory = []; CurrentCoord = None }

    Assert.False(isValidMove { Row = 1; Column = 1 } { Row = 3; Column = 3 } controller)

[<Fact>]
let ``Player cannot move two pieces in multijump``() =
    let board =
        array2D [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; InitialPosition = ""; MoveHistory = []; CurrentCoord = Some { Row = 2; Column = 2 } }

    Assert.False(isValidMove { Row = 0; Column = 2 } { Row = 2; Column = 0 } controller)

[<Fact>]
let ``Player can move one piece in multijump``() =
    let board =
        array2D [
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; Piece.whiteKing; None; None; None; None; None; None];
            [None; None; Piece.blackKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; InitialPosition = ""; MoveHistory = []; CurrentCoord = Some { Row = 2; Column = 2 } }

    Assert.True(isValidMove { Row = 2; Column = 2 } { Row = 0; Column = 0 } controller)

[<Fact>]
let ``Moving records move history: black hop``() =
    let controller = { Board = Board.defaultBoard; CurrentPlayer = Black; InitialPosition = ""; MoveHistory = []; CurrentCoord = None }
    let newController = movePiece { Row = 2; Column = 1 } { Row = 3; Column = 0 } controller

    Assert.Equal("9-13", (List.last newController.Value.MoveHistory).BlackMove.DisplayString)

[<Fact>]
let ``Moving records move history: black jump``() =
    let board =
        array2D [
            [None; Piece.blackKing; None; None; None; None; None; None];
            [None; None; Piece.whiteKing; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = Black; InitialPosition = ""; MoveHistory = []; CurrentCoord = None; }
    let newController = movePiece { Row = 0; Column = 1 } { Row = 2; Column = 3 } controller

    Assert.Equal("1x10", (List.last newController.Value.MoveHistory).BlackMove.DisplayString)

[<Fact>]
let ``Moving records move history: white hop``() =
    let board =
        array2D [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteKing; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = { Board = board; CurrentPlayer = White; InitialPosition = ""; MoveHistory = [{MoveNumber = 1; BlackMove = { Move = [1; 10]; ResultingFen = ""; DisplayString = "1x10" }; WhiteMove = None }]; CurrentCoord = None }
    let newController = movePiece { Row = 3; Column = 4 } { Row = 1; Column = 2 } controller
    let lastMove = (List.last newController.Value.MoveHistory)

    Assert.Equal("1x10", lastMove.BlackMove.DisplayString)
    Assert.Equal("15x6", lastMove.WhiteMove.Value.DisplayString)

[<Fact>]
let ``Takeback black's move``() =
    let doMove moveSeq gameController =
        (move moveSeq gameController).Value

    let newController =
        GameController.newGame
        |> doMove [{Row = 2; Column = 1;}; {Row = 3; Column = 0}]
        |> doMove [{Row = 5; Column = 0;}; {Row = 4; Column = 1}]

    let takeBack = takeBackMove newController
    let expectedFen = "[FEN \"W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13\"]"
    let actualFen = createFen White takeBack.Board

    Assert.Equal(expectedFen, actualFen)

[<Fact>]
let ``Takeback white's move``() =
    let doMove moveSeq gameController =
        (move moveSeq gameController).Value

    let newController =
        GameController.newGame
        |> doMove [{Row = 2; Column = 1;}; {Row = 3; Column = 0}]
        |> doMove [{Row = 5; Column = 0;}; {Row = 4; Column = 1}]
        |> doMove [{Row = 2; Column = 3;}; {Row = 3; Column = 2}]

    let takeBack = takeBackMove newController
    let expectedFen = "[FEN \"W:W17,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13\"]"
    let actualFen = createFen White takeBack.Board

    Assert.Equal(expectedFen, actualFen)

[<Fact>]
let ``Takeback move--no previous move``() =
    let newController = GameController.newGame

    let takeBack = takeBackMove newController
    let actualFen = createFen Black takeBack.Board

    Assert.Equal(Board.defaultFen, actualFen)

[<Fact>]
let ``Takeback move--single move``() =
    let doMove move gameController =
        (PublicAPI.move move gameController).Value

    let newController =
        GameController.newGame
        |> doMove [{Row = 2; Column = 1;}; {Row = 3; Column = 0}]

    let takeBack = takeBackMove newController
    let actualFen = createFen Black takeBack.Board

    Assert.Equal(Board.defaultFen, actualFen)