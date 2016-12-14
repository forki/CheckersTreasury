module BoardTests

open Checkers
open Checkers.Extensions
open Checkers.FSharpExtensions
open Xunit

[<Fact>]
let ``Starting coord row must exist``() =
    let board = new Board()
    Assert.False(board.IsValidMove({Row = -1; Column = 0}, {Row = 0; Column = 0}))

[<Fact>]
let ``Starting coord column must exist``() =
    let board = new Board()
    Assert.False(board.IsValidMove({Row = 0; Column = -1}, {Row = 0; Column = 0}))

[<Fact>]
let ``Ending coord row must exist``() =
    let board = new Board()
    Assert.False(board.IsValidMove({Row = 0; Column = 0}, {Row = -1; Column = 0}))

[<Fact>]
let ``Ending coord column must exist``() =
    let board = new Board()
    Assert.False(board.IsValidMove({Row = 0; Column = 0}, {Row = 0; Column = -1}))

[<Fact>]
let ``Move must be diagonal``() =
    Assert.False(moveIsDiagonal({Row = 2; Column = 1}, {Row = 3; Column = 1}))

[<Fact>]
let ``End coord must not match start coord``() =
    Assert.False(moveIsDiagonal({Row = 2; Column = 1}, {Row = 2; Column = 1}))
    
[<Fact>]
let ``Up right moves allowed``() =
    Assert.True(moveIsDiagonal({Row = 2; Column = 1}, {Row = 3; Column = 2}))

[<Fact>]
let ``Up left moves allowed``() =
    Assert.True(moveIsDiagonal({Row = 2; Column = 1}, {Row = 3; Column = 0}))
    
[<Fact>]
let ``Down right moves allowed``() =
    Assert.True(moveIsDiagonal({Row = 2; Column = 1}, {Row = 1; Column = 2}))

[<Fact>]
let ``Down left moves``() =
    Assert.True(moveIsDiagonal({Row = 2; Column = 1}, {Row = 1; Column = 0}))

[<Fact>]
let ``Black checker can move forward to empty square``() =
    let board = new Board()
    Assert.True(board.IsValidCheckerHop({Row = 2; Column = 1}, {Row = 3; Column = 0}))

[<Fact>]
let ``Black checker cannot move backward``() =
    let boardArray =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; None; None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];
    let board = new Board(boardArray)

    Assert.False(board.IsValidCheckerHop({Row = 3; Column = 0}, {Row = 2; Column = 1}))

[<Fact>]
let ``White checker can move forward to empty square``() =
    let board = new Board()
    Assert.True(board.IsValidCheckerHop({Row = 5; Column = 0}, {Row = 4; Column = 1}))

[<Fact>]
let ``White checker cannot move backward``() =
    let boardArray =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [None; None; None; None; None; None; None; None];
            [None; Piece.WhiteChecker(); None; None; None; None; None; None];
            [None; None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];
    let board = new Board(boardArray)

    Assert.False(board.IsValidCheckerHop({Row = 4; Column = 1}, {Row = 5; Column = 0}))

[<Fact>]
let ``Checker cannot move forward to claimed square``() =
    let board = new Board()
    Assert.False(board.IsValidCheckerHop({Row = 1; Column = 0}, {Row = 2; Column = 1}))

[<Fact>]
let ``Checker can jump forward``() =
    let boardArray =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [None; None; Piece.WhiteChecker(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];
    let board = new Board(boardArray)

    Assert.True(board.IsValidCheckerJump({Row = 2; Column = 1}, {Row = 4; Column = 3}))

[<Fact>]
let ``Checker cannot jump empty square``() =
    let boardArray =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];
    let board = new Board(boardArray)

    Assert.False(board.IsValidCheckerJump({Row = 2; Column = 1}, {Row = 4; Column = 3}))

[<Fact>]
let ``Checker cannot jump friend``() =
    let boardArray =
        [
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None];
            [None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker(); None; Piece.BlackChecker()];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
            [None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker()];
            [Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None; Piece.WhiteChecker(); None];
        ];
    let board = new Board(boardArray)

    Assert.False(board.IsValidCheckerJump({Row = 1; Column = 0}, {Row = 3; Column = 2}))

[<Fact>]
let ``King can move forward to empty square``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)
    Assert.True(board.IsValidCheckerHop({Row = 4; Column = 5}, {Row = 5; Column = 6}))

[<Fact>]
let ``King can move backward to empty square``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)
    Assert.True(board.IsValidCheckerHop({Row = 4; Column = 5}, {Row = 3; Column = 4}))

[<Fact>]
let ``King cannot move to claimed square``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; Piece.WhiteChecker(); None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)
    Assert.False(board.IsValidCheckerHop({Row = 4; Column = 5}, {Row = 5; Column = 6}))

[<Fact>]
let ``King can jump``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; Piece.WhiteChecker(); None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    Assert.True(board.IsValidCheckerJump({Row = 4; Column = 5}, {Row = 6; Column = 7}))

[<Fact>]
let ``King cannot jump empty square``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; Piece.WhiteChecker(); None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)
    
    Assert.False(board.IsValidCheckerJump({Row = 4; Column = 5}, {Row = 6; Column = 3}))

[<Fact>]
let ``King cannot jump friend``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; Piece.BlackKing(); None; None];
            [None; None; None; None; None; None; Piece.BlackChecker(); None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)
    
    Assert.False(board.IsValidCheckerJump({Row = 4; Column = 5}, {Row = 6; Column = 7}))

[<Fact>]
let ``setPieceAt places piece``() =
    let boardArray =
        [
            [None; Piece.WhiteKing(); None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; Piece.WhiteKing(); None; None; None; None; None; None];
            [None; None; None; None; Piece.BlackKing(); None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.SetPieceAt({Row = 1; Column = 4}, Piece.BlackKing())
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move hops piece``() =
    let boardArray =
        [
            [None; Piece.WhiteKing(); None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.WhiteKing(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 0; Column = 1}, {Row = 1; Column = 2}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Hopping black to line 7 promotes``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.BlackChecker(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; Piece.BlackKing(); None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 6; Column = 0}, {Row = 7; Column = 1}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Hopping white to line 0 promotes``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.WhiteChecker(); None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 1; Column = 1}, {Row = 0; Column = 0}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Jumping black to line 7 promotes``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.BlackChecker(); None; None; None; None; None; None; None];
            [None; Piece.WhiteChecker(); None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; Piece.BlackKing(); None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 5; Column = 0}, {Row = 7; Column = 2}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Jumping white to line 0 promotes``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.BlackChecker(); None; None; None; None; None; None];
            [None; None; Piece.WhiteChecker(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 2; Column = 2}, {Row = 0; Column = 0}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move jump down right jumps piece``() =
    let boardArray =
        [
            [None; Piece.WhiteKing(); None; None; None; None; None; None];
            [None; None; Piece.BlackKing(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; Piece.WhiteKing(); None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 0; Column = 1}, {Row = 2; Column = 3}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move jump down left jumps piece``() =
    let boardArray =
        [
            [None; None; Piece.WhiteKing(); None; None; None; None; None];
            [None; Piece.BlackKing(); None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 0; Column = 2}, {Row = 2; Column = 0}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move jump up right jumps piece``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.BlackKing(); None; None; None; None; None; None];
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; Piece.WhiteKing(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 2; Column = 0}, {Row = 0; Column = 2}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move jump up left jumps piece``() =
    let boardArray =
        [
            [None; None; None; None; None; None; None; None];
            [None; Piece.BlackKing(); None; None; None; None; None; None];
            [None; None; Piece.WhiteKing(); None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move({Row = 2; Column = 2}, {Row = 0; Column = 0}).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)

[<Fact>]
let ``Move sequence jumps pieces``() =
    let boardArray =
        [
            [Piece.WhiteKing(); None; None; None; None; None; None; None];
            [None; Piece.BlackKing(); None; Piece.BlackKing(); None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let board = new Board(boardArray)

    let expectedBoardArray =
        [
            [None; None; None; None; Piece.WhiteKing(); None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
            [None; None; None; None; None; None; None; None];
        ];
    let expectedBoard = new Board(expectedBoardArray)
    
    let newBoard = board.Move([{Row = 0; Column = 0}; {Row = 2; Column = 2}; {Row = 0; Column = 4}]).Value
    Assert.Equal<List<List<Option<Piece>>>>(expectedBoard.Board, newBoard.Board)