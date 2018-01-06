module Main exposing (..)

import Html exposing (text, div, button, program, Html, node, i)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute, class)
import RPS exposing (Move(..), winsWith )
import RPS.AI exposing (constAI, randomAI, winningAI)
import Task
import Random

type Message = Restart
             | Play Move
             | GameFinished Move Move

type GameResult = PlayerWon
                | BotWon
                | Draw

type alias FinishedGame =
    { playerMove : Move
    , botMove : Move
    , result : GameResult }

type alias Model = 
    { score : Score
    , finishedGames : List FinishedGame }

type alias Score = 
    { playerScore : Int
    , botScore : Int }

cmdLift : a -> Cmd a
cmdLift = Task.succeed >> Task.perform identity

constBot : (Move -> Cmd Message)
constBot =  makeBot (\_ -> constAI Rock) >> cmdLift

randomBot : (Move -> Cmd Message)
randomBot m = Random.generate (GameFinished m) randomAI

cheatingBot : (Move -> Cmd Message)
cheatingBot = makeBot winningAI >> cmdLift

emptyModel : Model 
emptyModel = Model zeroScore []

zeroScore : Score
zeroScore = Score 0 0

wins : Move -> Move -> Bool
wins = RPS.winsWith

makeBot : (Move -> Move) -> Move -> Message
makeBot f m = GameFinished m (f m)

decideWhoWon : Move -> Move -> GameResult
decideWhoWon player ai =
    if player == ai then
        Draw
    else if player |> wins ai then
        BotWon
    else 
        PlayerWon

scoreGame : Score -> GameResult -> Score
scoreGame score result = 
    case result of
        PlayerWon -> { score | playerScore = score.playerScore + 1 }
        BotWon -> { score | botScore = score.botScore + 1 }
        Draw -> score

update : (Move -> Cmd Message)  -- Bot Function
      -> Message            
      -> Model 
      -> (Model, Cmd Message)
update ai msg model = 
    case msg of 
        Restart -> (emptyModel, Cmd.none)
        Play move -> ( model , ai move)
        GameFinished player ai ->
            let
                result = decideWhoWon player ai 
            in
               ( { model 
                   | score = scoreGame model.score result
                   , finishedGames = (FinishedGame player ai result) :: model.finishedGames
               }, Cmd.none)

stylesheet : String -> Html msg
stylesheet link = 
    let 
        tag = "link"
        attrs = 
            [ attribute "rel" "stylesheet"
            , attribute "property" "stylesheet"
            , attribute "href" link
            ]
    in 
       node tag attrs []

view : Model -> Html Message
view model = 
    div []
    [ stylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    , stylesheet "content/style.css"
    , div [class "board"]
        [ div [class "button", onClick <| Play Rock] [i [class "fa fa-hand-rock-o"] []]
        , div [class "button", onClick <| Play Scissors] [i [class "fa fa-hand-scissors-o"] []] 
        , div [class "button", onClick <| Play Paper] [i [class "fa fa-hand-paper-o"] []]
        , div [class "score"] [ text <| toString model.score.playerScore, text ":" , text <| toString model.score.botScore]
        , div [class "restart button", onClick Restart] [text "Restart"]
        , div [class "finished-games"] <| List.map displayFinishedGame model.finishedGames
        ] 
    ]

displayFinishedGame : FinishedGame -> Html Message
displayFinishedGame game = 
    div [class "finished-game"] 
        [ text "Player: "
        , displayMove game.playerMove
        , text "Bot: "
        , displayMove game.botMove
        ]

moveToFaGlyph : Move -> String
moveToFaGlyph m = case m of
    Rock -> "fa fa-hand-rock-o"
    Paper -> "fa fa-hand-paper-o"
    Scissors -> "fa fa-hand-scissors-o"

displayMove : Move -> Html Message
displayMove m = i [class <| moveToFaGlyph m] []

main : Program Never Model Message
main = program
        { init = (emptyModel, Cmd.none)
        , update = update <| cheatingBot
        , view = view
        , subscriptions = \_ -> Sub.none }
