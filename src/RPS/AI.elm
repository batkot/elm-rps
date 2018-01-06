module RPS.AI exposing 
    ( constAI
    , cheatingAI
    , randomAI)

import Random exposing (Generator)

import RPS exposing (Move(..), winningMoves)

constAI : Move -> Move -> Move
constAI a _ = a

fst : (a,b) -> a
fst (a,b) = a

lookUpAI : List (Move, Move) -> Move-> Move -> Move
lookUpAI lookup default move =
    List.filter (\(_, snd) -> snd == move) lookup
    |> List.map fst
    |> List.head 
    |> Maybe.withDefault default

cheatingAI : Move -> Move
cheatingAI = lookUpAI winningMoves Scissors 

moveGen : Generator Move
moveGen = 
    let
        makeMove x = case x of
            1 -> Rock
            2 -> Scissors
            _ -> Paper
    in
        Random.map makeMove <| Random.int 1 3

randomAI : Move -> Generator Move
randomAI _ = moveGen
