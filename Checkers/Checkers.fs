module public Checkers.Generic
type Player = Black | White

type PieceType = Checker | King

type Coord = { Row :int; Column :int }

let offset c1 c2 =
   { Row = c1.Row + c2.Row; Column = c1.Column + c2.Column }

type Move = Coord List
type PdnMove = { Move :int List; ResultingFen :string; DisplayString :string }
type PdnTurn = { MoveNumber :int; BlackMove :PdnMove; WhiteMove :PdnMove Option }

type internal MoveTree = { Move :Move; Parent :Option<MoveTree>; Children :Option<List<MoveTree>> }
type internal AlphaBetaMove = { Alpha :float Option; Beta :float Option; Move :Move }

let listFromSeq (value :'a seq) =
    Some (List.ofSeq value)

let nestedListFromSeq (value :'a seq seq) =
    List.ofSeq (Seq.choose listFromSeq value)