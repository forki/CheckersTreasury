namespace Checkers

type Player = Black | White

type PieceType = Checker | King

type Coord = {Row: int; Column :int}
    with
    member coord.Exists() =
        coord.Row >= 0 && coord.Row <= 7 &&
        coord.Column >= 0 && coord.Column <= 7