module RPS.AISpecs exposing (tests)

import Fuzz 
import Test exposing (..)
import Expect exposing (Expectation)

import Fuzzers exposing (moveFuzzer)

import RPS
import RPS.AI exposing (constAI, cheatingAI)

tests : Test
tests = describe "RPS AI Tests" 
    [ fuzz2 moveFuzzer moveFuzzer "ConstAI should always return given move" prop_constAI
    , fuzz moveFuzzer "Cheating AI always returns winning move" prop_cheatingAIAlwaysWins
    ]

-- Const AI
prop_constAI : RPS.Move -> RPS.Move -> Expectation
prop_constAI given other =
    let 
        ai = constAI given
    in  
        ai other
        |> Expect.equal given

-- Cheating AI
prop_cheatingAIAlwaysWins : RPS.Move -> Expectation
prop_cheatingAIAlwaysWins otherMove = 
    let
        aiMove = cheatingAI otherMove
    in
        RPS.winsWith aiMove otherMove
        |> Expect.true "Cheating AI should win" 
