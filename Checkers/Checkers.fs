module public Checkers.Types
type Player = Black | White

type PieceType = Checker | King

type Coord = { Row :int; Column :int }

let offset c1 c2 =
   { Row = c1.Row + c2.Row; Column = c1.Column + c2.Column }

type Move = Coord List
type PDNMove = { Move :int List; ResultingFen :string }

type PDNTurn = { MoveNumber :int; BlackMove :PDNMove; WhiteMove :PDNMove Option; DisplayString :string }

type internal MoveTree = { Move :Move; Parent :Option<MoveTree>; Children :Option<List<MoveTree>> }
type internal AlphaBetaMove = { Alpha :float Option; Beta :float Option; Move :Move }