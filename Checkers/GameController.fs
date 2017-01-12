module public Checkers.GameController
open Checkers.Generic
open Checkers.Board

type GameController = { Variant :Variant; Board :Board; CurrentPlayer :Player; InitialPosition :string; MoveHistory :PdnTurn List; CurrentCoord :Option<Coord>; }