module RPS.AI exposing 
    ( constAI
    , cheatingAI)

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
