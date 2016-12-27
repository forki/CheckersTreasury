module internal Checkers.Minimax
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open System

let rec internal bestMatchInList player highestDifference moveForHighestDifference (list :List<float * Move>) =
    let head::tail = list
    let weight = fst head

    let newMoveForHighestDifference =
        match player with
        | Black -> match weight > highestDifference with
                   | true -> snd head
                   | false -> moveForHighestDifference
        | White -> match weight < highestDifference with
                   | true -> snd head
                   | false -> moveForHighestDifference

    let newHighestDifference =
        (highestDifference, weight)
        ||> match player with
            | Black -> max
            | White -> min

    match tail with
    | [] -> (highestDifference, newMoveForHighestDifference)
    | _ -> bestMatchInList player newHighestDifference newMoveForHighestDifference list.Tail

let internal chooseNewAlpha currentAlpha (candidateAlpha :float Option) =
    match currentAlpha with
    | Some x -> if candidateAlpha.IsSome then Some <| max x candidateAlpha.Value else currentAlpha
    | None -> candidateAlpha

let internal chooseNewBeta currentBeta (candidateBeta :float Option) =
    match currentBeta with
    | Some x -> if candidateBeta.IsSome then Some <| min x candidateBeta.Value else currentBeta
    | None -> candidateBeta

let rec minimax player searchDepth alpha beta (board :Board) =
    match searchDepth = 0 || (isWon board).IsSome with
    | true ->
        let weightDifference = Some <| calculateWeightDifference board

        let newAlpha = if player = Black then weightDifference else alpha
        let newBeta = if player = White then weightDifference else beta

        { Alpha = newBeta; Beta = newAlpha; Move = [] }
    | false ->
        let moves = calculateMoves player board
        let mutable alphaForNode = None
        let mutable betaForNode = None

        let mutable newAlpha = alpha
        let mutable newBeta = beta
        let mutable move = []

        if searchDepth <> 0 then
            ignore <| List.map (fun x -> if newAlpha.IsNone || newBeta.IsNone || newAlpha.Value < newBeta.Value then
                                             let newBoard = uncheckedMoveSequence x board
                                             let alphaBetaMove = minimax (otherPlayer player) (searchDepth - 1) alphaForNode betaForNode newBoard
                                             
                                             match player with
                                             | Black ->
                                                 alphaForNode <- chooseNewAlpha alphaForNode alphaBetaMove.Alpha
                                                 newAlpha <- chooseNewAlpha newAlpha alphaForNode
                                                 move <- if newAlpha = alphaBetaMove.Alpha then x else move
                                             | White ->
                                                 betaForNode <- chooseNewBeta betaForNode alphaBetaMove.Beta
                                                 newBeta <- chooseNewBeta newBeta betaForNode
                                                 move <- if newBeta = alphaBetaMove.Beta then x else move

                                         ())
                               moves

        { Alpha = betaForNode; Beta = alphaForNode; Move = move }