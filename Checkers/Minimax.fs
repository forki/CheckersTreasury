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

// When the player = Black, the node is a Max node
// When the search depth is 0 or the board is won, return the value
// When the node is a max node, get the best value for the nodes below it
// if the new value is higher than the minimum, set the temp value to the new value
// if the temp value is higher than the maximum value, return the max and an empty move
// Use the same principles for min nodes, except the opposite
let rec minimax player searchDepth alpha beta (board :Board) =
    let moves = calculateMoves player board

    let opponentMoves =
        match searchDepth = 0 with
        | false -> List.map (fun x -> let newBoard = uncheckedMoveSequence x board
                                      minimax (otherPlayer player) (searchDepth - 1) alpha beta newBoard)
                            moves
        | true -> List.empty

    let weightedMoves = List.mapi (fun i m -> (calculateWeightDifference (match (opponentMoves.IsEmpty) with
                                                                          | true -> uncheckedMoveSequence m board
                                                                          | false -> let newBoard = uncheckedMoveSequence m board
                                                                                     uncheckedMoveSequence opponentMoves.[i] newBoard), m)) moves

    snd (bestMatchInList player (fst weightedMoves.Head) (snd weightedMoves.Head) weightedMoves)