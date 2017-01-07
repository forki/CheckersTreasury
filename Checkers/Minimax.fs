module internal Checkers.Minimax
open Checkers.Generic
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI

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
    match (currentAlpha, candidateAlpha) with
    | (Some current, Some candidate) -> Some <| max current candidate
    | (Some current, None) -> Some current
    | (None, Some candidate) -> Some candidate
    | _ -> None

let internal chooseNewBeta currentBeta (candidateBeta :float Option) =
    match (currentBeta, candidateBeta) with
    | (Some current, Some candidate) -> Some <| min current candidate
    | (Some current, None) -> Some current
    | (None, Some candidate) -> Some candidate
    | _ -> None

let rec minimax player currentSearchDepth searchDepth alpha beta (board :Board) =
    match currentSearchDepth = 0 || (isWon board).IsSome with
    | true ->
        let weightDifference = Some <| calculateWeightDifference board
        let newAlpha = 
            match player with
            | Black -> weightDifference
            | _ -> alpha
        let newBeta =
            match player with
            | White -> weightDifference
            | _ -> beta
        { Alpha = newBeta; Beta = newAlpha; Move = [] }
    | false ->
        let getNewValueAndMove chooseMethod nodeValue moveValue currentValue currentMove newMove =
            let newNodeValue = chooseMethod nodeValue moveValue
            let newValue = chooseMethod currentValue newNodeValue
            let finalMove = 
                match newValue with
                | a when a = moveValue -> currentMove
                | _ -> newMove
            (newNodeValue, newValue, finalMove)

        let rec loop alphaForNode betaForNode (newAlpha :float Option) (newBeta :float Option) move moves =
            match moves with
            | [] -> { Alpha = betaForNode; Beta = alphaForNode; Move = move }
            | _ ->
                let currentMove = moves |> List.head
                match newAlpha.IsNone || newBeta.IsNone || newAlpha.Value < newBeta.Value with
                | false -> loop alphaForNode betaForNode newAlpha newBeta move (moves |> List.tail)
                | true ->
                    let newBoard = uncheckedMoveSequence currentMove board

                    let alphaBetaMove = minimax (otherPlayer player) (currentSearchDepth - 1) searchDepth alphaForNode betaForNode newBoard

                    match player with
                    | Black ->
                        let (newAlphaForNode, newNewAlpha, newMove) = getNewValueAndMove chooseNewAlpha alphaForNode alphaBetaMove.Alpha newAlpha currentMove move
                        loop newAlphaForNode betaForNode newNewAlpha newBeta newMove (moves |> List.tail)
                    | White ->
                        let (newBetaForNode, newNewBeta, newMove) = getNewValueAndMove chooseNewBeta betaForNode alphaBetaMove.Beta newBeta currentMove move
                        loop alphaForNode newBetaForNode newAlpha newNewBeta newMove (moves |> List.tail)

        let potentialMoves = (calculateMoves player board)
        match potentialMoves.Length, currentSearchDepth = searchDepth with
        | 1, true -> {Alpha = None; Beta = None; Move = potentialMoves.[0]}
        | _ -> loop None None alpha beta [] potentialMoves