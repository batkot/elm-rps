module Fuzzers exposing 
    ( oneOfValues 
    , moveFuzzer
    , twoDistinctMoves
    )

import Fuzz exposing (Fuzzer)
import RPS exposing (Move(..))

oneOfValues : List a -> Fuzzer a
oneOfValues =
    List.map Fuzz.constant 
    >> Fuzz.oneOf

-- Move
allMoves : List RPS.Move
allMoves = [RPS.Rock, RPS.Scissors, RPS.Paper]

moveFuzzer : Fuzzer RPS.Move
moveFuzzer = oneOfValues allMoves

twoDistinctValues : Fuzzer RPS.Move -> Fuzzer (RPS.Move, RPS.Move)
twoDistinctValues = 
    let
        except x = allMoves
            |> List.filter ((/=) x)
            |> oneOfValues
            |> Fuzz.map (\y -> (x,y))
    in 
        Fuzz.andThen except

twoDistinctMoves : Fuzzer (RPS.Move, RPS.Move)
twoDistinctMoves = twoDistinctValues moveFuzzer
