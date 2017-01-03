module public Checkers.GameController
open Checkers.Types
open Checkers.Board

type GameController = { Board :Board; CurrentPlayer :Player; CurrentCoord :Option<Coord>; MoveHistory :PDNTurn List }

let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None; MoveHistory = [] }