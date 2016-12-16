namespace Checkers

open Checkers.Types
open Checkers.Board

module public GameController =
    type GameController = { Board :Board; Player :Player }