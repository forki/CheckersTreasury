module public Checkers.PublicAPI
open Checkers.Variants
open Checkers.Types
open Checkers.Board
open Checkers.FSharpExtensions
open Checkers.Variants.AmericanCheckers
open Checkers.AIs.AmericanCheckersAI
open Checkers.GameController
open System

let isValidMove startCoord endCoord gameController =
    isValidMove startCoord endCoord gameController.Board &&
    (square startCoord gameController.Board).Value.Player = gameController.CurrentPlayer &&
    match gameController.CurrentCoord with
    | None -> true
    | coord -> startCoord = coord.Value

let movePiece startCoord endCoord gameController :Option<GameController> =
    let newBoard = movePiece startCoord endCoord gameController.Board

    match (isValidMove startCoord endCoord gameController) with
    | true -> Some <|
                {
                    Board = newBoard.Value;
                    CurrentPlayer = match playerTurnEnds startCoord endCoord gameController.Board newBoard.Value with
                                    | true -> otherPlayer gameController.CurrentPlayer
                                    | false -> gameController.CurrentPlayer        
                    CurrentCoord = match playerTurnEnds startCoord endCoord gameController.Board newBoard.Value with
                                    | true -> None
                                    | false -> Some endCoord
                }
    | false -> None

let move (moves :System.Collections.Generic.IEnumerable<Coord>) (gameController) :Option<GameController> =
    let board = moveSequence moves (Some gameController.Board)
    match board with
    | Some b -> Some {Board = board.Value; CurrentPlayer = otherPlayer gameController.CurrentPlayer; CurrentCoord = gameController.CurrentCoord}
    | None -> None

let rec bestMatchInList player highestDifference moveForHighestDifference (list :List<float * Move>) =
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

let rec getMove player (searchDepth :int) alpha beta (board :Board) :AlphaBetaMove =
    System.Diagnostics.Debug.WriteLine(String.Format("Alpha: {0};  Beta: {1}", alpha, beta))
    match alpha >= beta with
    | true -> {Alpha = alpha; Beta = beta; Move = []}
    | false ->
        let moves = calculateMoves player board

        let wonBoards = List.map (fun x -> (isWon (moveSequence x (Some board)).Value).IsSome) moves

        let moveWithOpponentResponse =
            match searchDepth = 0 || List.exists id wonBoards with
            | false ->
                       let mutable newAlpha = alpha
                       let mutable newBeta = beta
                       let opponentMoves = List.map (fun x ->
                                                        let alphaBetaMove = (getMove (otherPlayer player) (searchDepth - 1) newAlpha newBeta (moveSequence x (Some board)).Value)
                                                        newAlpha <- alphaBetaMove.Alpha
                                                        newBeta <- alphaBetaMove.Beta
                                                        x, alphaBetaMove.Move)
                                                    moves

                       List.where (fun (item :Move * Move) -> not (snd item).IsEmpty) opponentMoves
            | true -> List.empty

        System.Diagnostics.Debug.WriteLine(String.Format("Moves: {0};  Opponent moves: {1}", moves.Length, moveWithOpponentResponse.Length))

        let weightedMoves = 
                match moveWithOpponentResponse.IsEmpty with
                | false -> List.map (fun m -> (calculateWeightDifference (moveSequence (snd m) (moveSequence (fst m) (Some board))).Value), (fst m)) moveWithOpponentResponse
                | true -> List.map (fun m -> (calculateWeightDifference (moveSequence m (Some board)).Value, m)) moves
        
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

let isWon controller =
    isWon controller.Board