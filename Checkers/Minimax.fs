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

let internal chooseNewAlpha currentAlpha (candidateAlpha :float Option) =
    match currentAlpha with
    | Some x -> if candidateAlpha.IsSome then Some <| max x candidateAlpha.Value else currentAlpha
    | None -> candidateAlpha

let internal chooseNewBeta currentBeta (candidateBeta :float Option) =
    match currentBeta with
    | Some x -> if candidateBeta.IsSome then Some <| min x candidateBeta.Value else currentBeta
    | None -> candidateBeta

// When the player = Black, the node is a Max node
// When the search depth is 0 or the board is won, return the value                       - done
// When the node is a max node, get the best value for the nodes below it
// if the new value is higher than the minimum, set the temp value to the new value
// if the temp value is higher than the maximum value, return the max and an empty move
// Use the same principles for min nodes, except the opposite
let rec minimax player searchDepth (alpha :Option<float>) (beta :Option<float>) (board :Board) =
    match alpha.IsSome && beta.IsSome && alpha.Value >= beta.Value with
    | true -> { Alpha = None; Beta = None; Move = []}
    | false ->
        match searchDepth = 0 || (isWon board).IsSome with
        | true ->
            let weightDifference = Some <| calculateWeightDifference board
            let newAlpha =
                match player with
                | White -> chooseNewAlpha beta weightDifference
                | Black -> if beta.IsNone then None else chooseNewAlpha beta weightDifference

            let newBeta =
                match player with
                | Black -> chooseNewBeta alpha weightDifference
                | White -> if alpha.IsNone then None else chooseNewBeta alpha weightDifference

            { Alpha = newAlpha; Beta = newBeta; Move = [] }
        | false ->
            let moves = calculateMoves player board
            let mutable newAlpha = alpha
            let mutable newBeta = beta
            let mutable move = []

            if searchDepth <> 0 then
                ignore <| List.map (fun x -> let newBoard = uncheckedMoveSequence x board
                                             let alphaBetaMove = minimax (otherPlayer player) (searchDepth - 1) alpha beta newBoard
                                              
                                             if player = Black then
                                                newAlpha <- chooseNewAlpha newAlpha alphaBetaMove.Alpha

                                             if player = White then
                                                newBeta <- chooseNewBeta newBeta alphaBetaMove.Beta

                                             move <- if (player = Black && newAlpha = alphaBetaMove.Alpha) || (player = White && newBeta = alphaBetaMove.Beta) then
                                                        x
                                                     else
                                                        move
                                             ())
                                    moves

            { Alpha = newBeta; Beta = newAlpha; Move = move }