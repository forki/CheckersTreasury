module GameControllerTests
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

    let controller = { Board = board; CurrentPlayer = White; CurrentCoord = None; MoveHistory = [{MoveNumber = 1; BlackMove = { Move = [1; 10]; ResultingFen = "" }; WhiteMove = None; DisplayString = "1x10"}] }
    let newController = movePiece { Row = 3; Column = 4 } { Row = 1; Column = 2 } controller

    Assert.Equal("1: 1x10, 15x6", (List.last newController.Value.MoveHistory).DisplayString)

[<Fact>]
let ``Create controller from FEN string``() =
    let expectedBoard =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedPlayer = Player.White

    let fenString = "[FEN \"W:WK15,19:BK10\"]"
    let controller = controllerFromFen fenString
    
    Assert.Equal(expectedPlayer, controller.CurrentPlayer)
    Assert.Equal<Checkers.Board.Board>(expectedBoard, controller.Board)

[<Fact>]
let ``Create FEN from controller string``() =
    let board =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.blackKing; None; None; None; None];
            [None; None; None; None; Piece.whiteKing; None; None; None];
            [None; None; None; None; None; Piece.whiteChecker; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];

    let controller = {Board = board; CurrentPlayer = Player.White; CurrentCoord = None; MoveHistory = []}

    let expectedFENString = "[FEN \"W:WK15,19:BK10\"]"
    
    Assert.Equal(expectedFENString, (createFen Player.White board))

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