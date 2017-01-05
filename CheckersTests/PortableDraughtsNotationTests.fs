module PortableDraughtsNotationTests
open Checkers
open Checkers.GameController
open Checkers.PortableDraughtsNotation
open Checkers.Types
open Checkers.PublicAPI
open Xunit

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

    let expectedFENString = "[FEN \"W:WK15,19:BK10\"]"
    Assert.Equal(expectedFENString, (createFen Player.White board))

[<Fact>]
let ``Create controller from FEN string: white move does not crash``() =
    let fenString = "[FEN \"W:W21,22,23,24,25,26,27,28,29,30,31,32:B1,2,3,4,5,6,7,8,10,11,12,13\"]"
    let controller =
        fenString
        |> controllerFromFen
        |> movePiece {Row = 5; Column = 6} {Row = 4; Column = 7}

    let move = controller.Value.MoveHistory.Head
    
    Assert.Equal("…", move.BlackMove.DisplayString)
    Assert.Equal("24-20", move.WhiteMove.Value.DisplayString)