module RPSSpecs exposing (tests)

import RPS

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, map)
import Test exposing (..)
import Fuzzers exposing (oneOfValues, moveFuzzer, twoDistinctMoves)

tests : Test
tests = describe "RPS Game Logic tests" 
    [ fuzz moveFuzzer "Same moves should draw" prop_sameMovesShoudDraw
    , fuzz2 moveFuzzer moveFuzzer "Flipping moves should flip result" prop_gameIsSymetric
    , fuzz winningCombinations "PlayerOneResult for PlayerOne winning combination" spec_playerOneWins
    , fuzz winningCombinations "PlayerTwoResult for PlayerTwo winning combination" spec_playerTwoWins
    ]

winningCombinations : Fuzzer (RPS.Move, RPS.Move)
winningCombinations = oneOfValues RPS.winningMoves

prop_sameMovesShoudDraw : RPS.Move -> Expectation
prop_sameMovesShoudDraw m = 
    RPS.playGame m m
    |> Expect.equal RPS.Draw

prop_gameIsSymetric : RPS.Move -> RPS.Move -> Expectation
prop_gameIsSymetric x y =
    let
        matchA = RPS.playGame x y
        matchB = RPS.playGame y x
    in
        Expect.equal matchA <| flipResult matchB

flipResult : RPS.GameResult -> RPS.GameResult
flipResult x = case x of
    RPS.PlayerOneWon -> RPS.PlayerTwoWon
    RPS.PlayerTwoWon -> RPS.PlayerOneWon
    RPS.Draw -> RPS.Draw

spec_playerOneWins : (RPS.Move, RPS.Move) -> Expectation
spec_playerOneWins (win, lose) =
    RPS.playGame win lose
    |> Expect.equal RPS.PlayerOneWon

spec_playerTwoWins : (RPS.Move, RPS.Move) -> Expectation
spec_playerTwoWins (win, lose) = 
    RPS.playGame lose win 
    |> Expect.equal RPS.PlayerTwoWon


-- Kind'of redundant, not sure which API choose
