module RPS.AI exposing 
    ( constAI
    , winningAI
    , randomAI)

import Random exposing (Generator)

import RPS exposing (Move(..), winningMoves)

constAI : Move -> Move
constAI = identity

fst : (a,b) -> a
fst (a,b) = a

lookUpAI : List (Move, Move) -> Move-> Move -> Move
lookUpAI lookup default move =
    List.filter (\(_, snd) -> snd == move) lookup
    |> List.map fst
    |> List.head 
    |> Maybe.withDefault default

winningAI : Move -> Move
winningAI = lookUpAI winningMoves Scissors 

randomAI : Generator Move
randomAI = 
    let
        makeMove x = case x of
            1 -> Rock
            2 -> Scissors
            _ -> Paper
    in
        Random.map makeMove <| Random.int 1 3
