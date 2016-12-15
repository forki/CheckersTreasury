namespace Checkers

module Types =
    type Player = Black | White

    type PieceType = Checker | King

    type Coord = { Row :int; Column :int }

    let (+) coord1 coord2 =
        { Row = coord1.Row + coord2.Row; Column = coord1.Column + coord2.Column }