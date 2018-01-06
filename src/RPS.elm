module RPS exposing 
    ( Move(..)
    , winsWith
    , draws
    , winningMoves) 

type Move = Rock
          | Paper
          | Scissors

winningMoves : List (Move, Move)
winningMoves = 
        [(Rock, Scissors)
        ,(Scissors, Paper)
        ,(Paper, Rock)]

winsWith : Move -> Move -> Bool
winsWith x y = case (x,y) of
    (Rock, Scissors) -> True
    (Scissors, Paper) -> True
    (Paper, Rock) -> True
    _ -> False

draws : Move -> Move -> Bool
draws = (==)
