namespace Checkers

type Player = Black | White

type PieceType = Checker | King

type Coord = {
    Row :int
    Column :int
} with
    static member (+) (coord1 :Coord, coord2 :Coord) =
        {Row = coord1.Row + coord2.Row; Column = coord1.Column + coord2.Column}