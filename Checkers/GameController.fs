namespace Checkers

open Checkers.Types
open Checkers.Board

module public GameController =
    type GameController = { Board :Board; CurrentPlayer :Player; CurrentCoord :Option<Coord> }

    let newGame = { Board = Board.defaultBoard; CurrentPlayer = Black; CurrentCoord = None }