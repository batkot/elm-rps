module RPSSpecs exposing (tests)

import RPS

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, intRange, map)
import Test exposing (..)
import Fuzzers exposing (oneOfValues, moveFuzzer, twoDistinctMoves)

tests : Test
tests = describe "RPS Game Logic tests" 
    [ fuzz moveFuzzer "Same moves should draw" prop_sameMovesShoudDraw
    , fuzz2 moveFuzzer moveFuzzer "Draw should be commutative" prop_drawCommutative
    , fuzz twoDistinctMoves "Different moves lead to one players win" prop_onlyOneWinner
    , fuzz winningCombinations "Winning sequences" spec_winningCombinations
    ]

winningCombinations : Fuzzer (RPS.Move, RPS.Move)
winningCombinations = oneOfValues RPS.winningMoves

prop_sameMovesShoudDraw : RPS.Move -> Expectation
prop_sameMovesShoudDraw m = 
    RPS.draws m m 
    |> Expect.true "Expected draw"

prop_drawCommutative : RPS.Move -> RPS.Move -> Expectation
prop_drawCommutative x y =
    let
        one = RPS.draws x y
        two = RPS.draws y x
    in
        Expect.equal one two

prop_onlyOneWinner : (RPS.Move, RPS.Move) -- Two different moves
                  -> Expectation
prop_onlyOneWinner (x, y) =
    let
        xWins = RPS.winsWith x y
        yWins = RPS.winsWith y x
    in
       Expect.equal xWins (not yWins)

spec_winningCombinations : (RPS.Move, RPS.Move) -> Expectation
spec_winningCombinations (win, lose) =
    RPS.winsWith win lose
    |> Expect.true "Expect winning combination" 
