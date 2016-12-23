module internal Checkers.Minimax
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open System

let rec internal bestMatchInList player highestDifference moveForHighestDifference (list :List<float * Move>) =
    let weight = fst list.Head
    let newMoveForHighestDifference =
        match player with
        | Black -> match weight > highestDifference with
                   | true -> snd list.Head
                   | false -> moveForHighestDifference
        | White -> match weight < highestDifference with
                   | true -> snd list.Head
                   | false -> moveForHighestDifference

    let newHighestDifference =
        match player with
        | Black -> Math.Max(highestDifference, weight)
        | White -> Math.Min(highestDifference, weight)

    match list.Tail.IsEmpty with
    | false -> bestMatchInList player newHighestDifference newMoveForHighestDifference list.Tail
    | true -> (highestDifference, newMoveForHighestDifference)

let internal chooseNewAlpha player currentAlpha candidateAlpha =
    match player with
    | Black ->
        match currentAlpha > candidateAlpha with
        | true -> currentAlpha
        | false -> candidateAlpha
    | White ->
        match currentAlpha < candidateAlpha with
        | true -> currentAlpha
        | false -> candidateAlpha

let internal chooseNewBeta player currentBeta candidateBeta =
    match player with
    | Black ->
        match currentBeta < candidateBeta with
        | true -> currentBeta
        | false -> candidateBeta
    | White ->
        match currentBeta > candidateBeta with
        | true -> currentBeta
        | false -> candidateBeta

let rec minimax player (searchDepth :int) (board :Board) =
    let moves = calculateMoves player board

    let wonBoards = List.map (fun x -> (isWon (moveSequence x (Some board)).Value).IsSome) moves

    let opponentMoves =
        match searchDepth = 0 || List.exists id wonBoards with
        | false -> List.map (fun x -> minimax (otherPlayer player) (searchDepth - 1) (moveSequence x (Some board)).Value) moves
        | true -> List.empty

    let weightedMoves = List.mapi (fun i m -> (calculateWeightDifference (match (opponentMoves.IsEmpty) with
                                                                          | true -> (moveSequence m (Some board)).Value
                                                                          | false -> let newBoard = (moveSequence m (Some board)).Value
                                                                                     (moveSequence opponentMoves.[i] (Some newBoard)).Value),
                                                                       m)) moves

    let rec loop highestDifference moveForHighestDifference (list :List<float * Move>) =
        let weight = fst list.Head
        let newMoveForHighestWeight =
            match player with
            | Black -> match weight > highestDifference with
                       | true -> snd list.Head
                       | false -> moveForHighestDifference
            | White -> match weight < highestDifference with
                       | true -> snd list.Head
                       | false -> moveForHighestDifference

        let newHighestDifference =
            match player with
            | Black -> Math.Max(highestDifference, weight)
            | White -> Math.Min(highestDifference, weight)

        match list.Tail.IsEmpty with
        | false -> loop newHighestDifference newMoveForHighestWeight list.Tail
        | true -> newMoveForHighestWeight

    loop (fst weightedMoves.Head) (snd weightedMoves.Head) weightedMoves