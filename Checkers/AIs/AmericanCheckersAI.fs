module Checkers.AIs.AmericanCheckersAI

open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.Types
open System

let checkerWeights =
    [[0.0; 1.20; 0.0; 1.20; 0.0; 1.20; 0.0; 1.10];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [1.15; 0.0; 1.05; 0.0; 1.0; 0.0; 1.10; 0.0];
    [0.0; 1.10; 0.0; 1.0; 0.0; 1.05; 0.0; 1.15];
    [1.10; 0.0; 1.20; 0.0; 1.20; 0.0; 1.20; 0.0]]

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
    | _ -> Some {Row = coord.Row; Column = coord.Column + 1}

let calculateCheckerWeight coord (board :Board) =
    let piece = square coord board
    let kingRow = kingRowIndex piece.Value.Player
    8.0 - (float <| Math.Abs(kingRow - coord.Row)) + (square coord checkerWeights)

let calculateKingWeight coord =
    10.0 + (square coord kingWeights)

let calculatePieceWeight coord (board :Board) =
    let piece = square coord board
    match piece.Value.PieceType with
    | Checker -> calculateCheckerWeight coord board
    | King -> calculateKingWeight coord

let calculateWeight player (board :Board) =
    let rec loop (weight :float) coord :float =
        match nextPoint coord with
        | Some c ->
            match isPlayerPiece player coord board with
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

let getPieceJumps coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerJumps piece.Player
        | King -> kingJumps piece.Player

    let jumps = List.ofSeq (seq {
        for move in moves do
        let endCoord = (coord + move)
        yield
            match coordExists endCoord && isValidJump coord endCoord board with
            | true -> Some (coord, endCoord)
            | false -> None })

    List.map (fun (item :Option<Coord * Coord>) -> item.Value) (List.where (fun (item :Option<Coord * Coord>) -> item.IsSome) jumps)

let getPieceHops coord (board :Board) =
    let piece = (square coord board).Value
    let moves = 
        match piece.PieceType with
        | Checker -> checkerHops piece.Player
        | King -> kingHops piece.Player

    let hops = List.ofSeq (seq {
        for move in moves do
        let endCoord = (coord + move)
        yield
            match coordExists endCoord && isValidHop coord endCoord board with
            | true -> Some (coord, endCoord)
            | false -> None })

    List.map (fun (item :Option<Coord * Coord>) -> item.Value) (List.where (fun (item :Option<Coord * Coord>) -> item.IsSome) hops)

let calculateMoves player (board :Board) =
    let rec loop jumpAcc hopAcc coord =
        match isPlayerPiece player coord board with
        | true ->
            let newJumpAcc = getPieceJumps coord board @ jumpAcc
            match newJumpAcc.IsEmpty with
            | true ->
                let newHopAcc = getPieceHops coord board @ hopAcc
                match nextPoint coord with
                | Some c -> loop newJumpAcc newHopAcc c
                | None -> newHopAcc
            | false ->
                match nextPoint coord with
                | Some c -> loop newJumpAcc [] c
                | None -> newJumpAcc
        | false ->
            match nextPoint coord with
            | Some c -> loop jumpAcc hopAcc c
            | None -> jumpAcc @ hopAcc
    
    loop [] [] {Row = 0; Column = 0}

let rec getBestMove player (searchDepth :int) (board :Board) =
    let moves = calculateMoves player board

    let wonBoards = List.map (fun x -> (isWon (movePiece (fst x) (snd x) board).Value).IsSome) moves

    let opponentMoves =
        match searchDepth = 0 || List.exists id wonBoards with
        | false -> List.map (fun x -> getBestMove (otherPlayer player) (searchDepth - 1) (movePiece (fst x) (snd x) board).Value) moves
        | true -> List.empty

    let weightedMoves = List.mapi (fun i (m :Coord * Coord) -> (calculateWeight player (match (opponentMoves.IsEmpty) with
                                                                                        | true -> (movePiece (fst m) (snd m) board).Value
                                                                                        | false -> let newBoard = (movePiece (fst m) (snd m) board).Value
                                                                                                   (movePiece (fst opponentMoves.[i]) (snd opponentMoves.[i]) newBoard).Value)
                                                                , m)) moves

    let rec loop highestWeight moveForHighestWeight (list :List<float * (Coord * Coord)>) =
        let weight = fst list.Head
        let newMoveForHighestWeight =
            match weight >= highestWeight with
            | true -> snd list.Head
            | false -> moveForHighestWeight

        match list.Tail.IsEmpty with
        | false -> loop (Math.Max(highestWeight, weight)) newMoveForHighestWeight list.Tail
        | true -> newMoveForHighestWeight

    loop (fst weightedMoves.Head) (snd weightedMoves.Head) weightedMoves