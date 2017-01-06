module public Checkers.GameController
open Checkers.Types
open Checkers.Board

type GameController = { Board :Board; CurrentPlayer :Player; InitialPosition :string; MoveHistory :PDNTurn List; CurrentCoord :Option<Coord>; }

let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; InitialPosition = Board.defaultFen; MoveHistory = []; CurrentCoord = None }