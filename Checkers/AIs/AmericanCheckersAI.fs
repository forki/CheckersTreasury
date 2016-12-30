module Checkers.AIs.AmericanCheckersAI
open Checkers.Board
open Checkers.Variants.AmericanCheckers
open Checkers.Types
open System

let checkerWeights =
    [[0.0; 3.20; 0.0; 3.20; 0.0; 3.20; 0.0; 3.10];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [3.10; 0.0; 3.20; 0.0; 3.20; 0.0; 3.20; 0.0]]

let kingWeights =
    [[0.0; 1.05; 0.0; 1.0; 0.0; 1.0; 0.0; 1.0];
    [1.05; 0.0; 1.10; 0.0; 1.05; 0.0; 1.05; 0.0];
    [0.0; 1.10; 0.0; 1.15; 0.0; 1.10; 0.0; 1.0];
    [1.0; 0.0; 1.15; 0.0; 1.20; 0.0; 1.05; 0.0];
    [0.0; 1.05; 0.0; 1.20; 0.0; 1.15; 0.0; 1.0];
    [1.0; 0.0; 1.10; 0.0; 1.15; 0.0; 1.10; 0.0];
    [0.0; 1.05; 0.0; 1.05; 0.0; 1.10; 0.0; 1.05];
    [1.0; 0.0; 1.0; 0.0; 1.0; 0.0; 1.05; 0.0]]

let isPlayerPiece player coord (board :Board) =
    let piece = square coord board
    piece.IsSome && player = piece.Value.Player

let nextPoint coord =
    match coord with
    | c when c.Row = Rows && c.Column = Columns -> None
    | c when c.Column = Columns -> Some {Row = c.Row + 1; Column = 0}
    | _ -> Some {coord with Column = coord.Column + 1}

let calculateCheckerWeight coord (board :Board) =
    let piece = (square coord board).Value
    let kingRow = kingRowIndex piece.Player

    let weight = 8.0 - (float <| abs(kingRow - coord.Row)) + (square coord checkerWeights)
    match piece.Player with
    | Black -> weight
    | White -> -weight

let calculateKingWeight coord (board :Board) =
    let piece = (square coord board).Value
    let weight = 8.0 + (square coord kingWeights)

    match piece.Player with
    | Black -> weight
    | White -> -weight

let calculatePieceWeight coord (board :Board) =
    let piece = square coord board
    match piece.Value.PieceType with
    | Checker -> calculateCheckerWeight coord board
    | King -> calculateKingWeight coord board

let calculateWeight player (board :Board) =
    let rec loop (weight :float) coord :float =
        match nextPoint coord with
        | Some c ->
            match isPlayerPiece player coord board with
            | true -> loop (weight + (calculatePieceWeight coord board)) c
            | false -> loop weight c
        | None -> weight
    
    loop 0.0 {Row = 0; Column = 0}

let calculateWeightDifference (board :Board) =
    let rec loop (weight :float) coord =
        match nextPoint coord with
        | Some c ->
            let piece = square coord board
            match piece.IsSome with
            | true -> loop (weight + (calculatePieceWeight coord board)) c
            | false -> loop weight c
        | None -> weight
    
    loop 0.0 {Row = 0; Column = 0}

let checkerJumps player =
    match player with
    | White -> [{Row = -2; Column = -2}; {Row = -2; Column = 2}]
    | Black -> [{Row = 2; Column = -2}; {Row = 2; Column = 2}]

let kingJumps player =
    (checkerJumps player) @
        (match player with
        | White -> [{Row = 2; Column = -2}; {Row = 2; Column = 2}]
        | Black -> [{Row = -2; Column = -2}; {Row = -2; Column = 2}])

let checkerHops player =
    match player with
    | White -> [{Row = -1; Column = -1}; {Row = -1; Column = 1}]
    | Black -> [{Row = 1; Column = -1}; {Row = 1; Column = 1}]

let kingHops player =
    (checkerHops player) @
        (match player with
        | White -> [{Row = 1; Column = -1}; {Row = 1; Column = 1}]
        | Black -> [{Row = -1; Column = -1}; {Row = -1; Column = 1}])

let getPieceSingleJumps coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerJumps piece.Player
        | King -> kingJumps piece.Player

    let hops = List.ofSeq (seq {
        for move in moves do
        let endCoord = offset coord move
        yield
            match coordExists endCoord && isValidJump coord endCoord board with
            | true -> Some [coord; endCoord]
            | false -> None })

    List.map (fun (item :Option<Move>) -> item.Value) (List.where (fun (item :Option<Move>) -> item.IsSome) hops)

let rec internal createMoveTree (move :Move) (board :Board) =
    let moveTree =
        {
            Move = move;
            Parent = None;
            Children =
                let newBoard = if move.Length = 1 then board else uncheckedMoveSequence move board

                let newJumps = getPieceSingleJumps (List.last move) newBoard
                let newMoveEndCoords = List.map (fun item -> List.last item) newJumps

                let oldPieceType = (square move.Head board).Value.PieceType
                let newPieceType = (square (List.last move) newBoard).Value.PieceType

                match newMoveEndCoords.IsEmpty || (oldPieceType = Checker && newPieceType = King) with
                | false ->
                    let moves = List.map (fun (item :Coord) -> move @ [item]) newMoveEndCoords
                    let children = List.map (fun item -> createMoveTree item board) moves
                    Some children
                | true -> None
        }

    moveTree

let getPieceJumps coord (board :Board) =
    let moves = new System.Collections.Generic.List<Move>()

    let rec loop (moveTree :MoveTree) =
        match moveTree.Children with
        | None -> moves.Add(moveTree.Move)
        | Some t -> List.iter (fun item -> (loop item)) t

    let moveTree = createMoveTree [coord] board
    match moveTree.Children with
    | Some t -> loop <| createMoveTree [coord] board
    | None -> ()

    List.ofSeq moves

let getPieceHops coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerHops piece.Player
        | King -> kingHops piece.Player

    let hopsFilter = List.filter (fun (head::tail) ->
        let startCoord = head
        let endCoord = tail |> List.head
        coordExists endCoord && isValidHop startCoord endCoord board)

    moves |> List.map (fun move -> [coord; offset coord move]) |> hopsFilter

let calculateMoves player (board :Board) =
    let rec loop jumpAcc hopAcc coord =
        match isPlayerPiece player coord board with
        | true ->
            let newJumpAcc = getPieceJumps coord board @ jumpAcc
            match newJumpAcc with
            | [] ->
                let newHopAcc = getPieceHops coord board @ hopAcc
                match nextPoint coord with
                | Some c -> loop newJumpAcc newHopAcc c
                | None -> newHopAcc
            | _ ->
                match nextPoint coord with
                | Some c -> loop newJumpAcc [] c
                | None -> newJumpAcc
        | false ->
            match nextPoint coord with
            | Some c -> loop jumpAcc hopAcc c
            | None -> jumpAcc @ hopAcc
    
    loop [] [] {Row = 0; Column = 0}