module RPS exposing 
    ( Move(..)
    , winningMoves
    , GameResult(..)
    , playGame ) 

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

type GameResult = PlayerOneWon
                | PlayerTwoWon
                | Draw

playGame : Move -> Move -> GameResult
playGame one two = 
    if draws one two then
        Draw
    else if winsWith one two then
        PlayerOneWon
    else 
        PlayerTwoWon
