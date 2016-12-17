module public Checkers.GameController
open Checkers.Types
open Checkers.Board

type GameController = { Board :Board; CurrentPlayer :Player; CurrentCoord :Option<Coord> }

let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None }