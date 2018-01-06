module MainSpecs exposing (tests)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, intRange)
import Fuzzers exposing (oneOfValues)
import Test exposing (..)

import Main exposing (..)
import RPS exposing (GameResult(..))

resultFuzzer : Fuzzer GameResult
resultFuzzer = oneOfValues [PlayerOneWon, PlayerTwoWon, Draw]

scoreFuzzer : Fuzzer Score
scoreFuzzer = Fuzz.map2 Score Fuzz.int Fuzz.int

tests = describe "Main App Tests" 
    [ fuzz (list resultFuzzer) "Player score should add up to won games" prop_playerScoreShouldAddup
    , fuzz (list resultFuzzer) "Bot score should add up to won games" prop_botScoreShouldAddup
    , fuzz2 scoreFuzzer (intRange 1 100) "Draws shouldn't modify score" prop_drawShouldNotChangeScore
    ]

prop_playerScoreShouldAddup : List GameResult -> Expectation
prop_playerScoreShouldAddup games =
    let
        wonGames = 
            List.filter ((==) PlayerOneWon) games
            |> List.length
        score = List.foldr (flip scoreGame) zeroScore games
    in
        Expect.equal wonGames <| score.playerScore

prop_botScoreShouldAddup : List GameResult -> Expectation
prop_botScoreShouldAddup games =
    let
        wonGames = 
            List.filter ((==) PlayerTwoWon) games
            |> List.length
        score = List.foldr (flip scoreGame) zeroScore games
    in
        Expect.equal wonGames <| score.botScore

prop_drawShouldNotChangeScore : Score -> Int -> Expectation
prop_drawShouldNotChangeScore score count =
   List.range 0 count
   |> List.map (\_ -> Draw)
   |> List.foldr (flip scoreGame) score 
   |> Expect.equal score
