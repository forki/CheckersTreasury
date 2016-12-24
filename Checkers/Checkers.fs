module public Checkers.Types
type Player = Black | White

type PieceType = Checker | King

type Coord = { Row :int; Column :int }

type Coord with
    static member (+) (c1 :Coord, c2 :Coord) =
        { Row = c1.Row + c2.Row; Column = c1.Column + c2.Column }

type Move = Coord List

type MoveTree = { Move :Move; Parent :Option<MoveTree>; Children :Option<List<MoveTree>> }

type internal AlphaBetaMove = { Alpha :float Option; Beta :float Option; Move :Move }