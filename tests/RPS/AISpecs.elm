module RPS.AISpecs exposing (tests)

import Fuzz 
import Test exposing (..)
import Expect exposing (Expectation)

import Fuzzers exposing (moveFuzzer)

import RPS 
import RPS.AI exposing (constAI, winningAI)

tests : Test
tests = describe "RPS AI Tests" 
    [ fuzz moveFuzzer "ConstAI should always return given move" prop_constAI
    , fuzz moveFuzzer "Cheating AI always returns winning move" prop_cheatingAIAlwaysWins
    ]

-- Const AI
prop_constAI : RPS.Move -> Expectation
prop_constAI given =
        constAI given
        |> Expect.equal given

-- Cheating AI
prop_cheatingAIAlwaysWins : RPS.Move -> Expectation
prop_cheatingAIAlwaysWins otherMove = 
    let
        aiMove = winningAI otherMove
    in
        RPS.playGame aiMove otherMove
        |> Expect.equal RPS.PlayerOneWon
