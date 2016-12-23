module internal Checkers.Minimax
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open Checkers.GameController
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

let rec internal minimax player (searchDepth :int) alpha beta (board :Board) :AlphaBetaMove =
    match alpha >= beta with
    | true -> {Alpha = alpha; Beta = beta; Move = []}
    | false ->
        let moves = calculateMoves player board

        let wonBoards = List.map (fun x -> (isWon (uncheckedMoveSequence x board)).IsSome) moves

        let moveWithOpponentResponse =
            match searchDepth = 0 || moves.Length = 1 || List.exists id wonBoards with
            | false -> let mutable newAlpha = alpha
                       let mutable newBeta = beta
                       let opponentMoves = List.map (fun x ->
                                                        let alphaBetaMove = (minimax (otherPlayer player) (searchDepth - 1) newAlpha newBeta (uncheckedMoveSequence x board))
                                                        newAlpha <- chooseNewAlpha player newAlpha alphaBetaMove.Alpha
                                                        newBeta <- chooseNewAlpha player newBeta alphaBetaMove.Beta
                                                        x, alphaBetaMove.Move)
                                                    moves

                       List.where (fun (item :Move * Move) -> not (snd item).IsEmpty) opponentMoves
            | true -> List.empty

        let weightedMoves = 
                match moveWithOpponentResponse.IsEmpty with
                | false -> List.map (fun m -> (calculateWeightDifference (uncheckedMoveSequence (snd m) (uncheckedMoveSequence (fst m) board))), (fst m)) moveWithOpponentResponse
                | true -> List.map (fun m -> (calculateWeightDifference (uncheckedMoveSequence m board), m)) moves
        
        match weightedMoves.IsEmpty with
        | true -> {
                      Alpha = if Double.IsInfinity(beta) then alpha else beta;
                      Beta = if Double.IsInfinity(alpha) then beta else alpha;
                      Move = []
                  }
        | false ->
            let weightedMove = bestMatchInList player (fst weightedMoves.Head) (snd weightedMoves.Head) weightedMoves
            {
                Alpha = fst weightedMove;
                Beta = if Double.IsInfinity(alpha) then beta else alpha;
                Move = snd weightedMove
            }