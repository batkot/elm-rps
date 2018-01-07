module Main exposing (..)

import Html exposing (text, div, button, program, Html, node, i, h1, h2)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute, class)
import RPS exposing (Move(..), GameResult(..), playGame)
import RPS.AI exposing (constAI, randomAI, winningAI)
import Task
import Random

type Message = Restart
             | Play Move
             | GameFinished Move Move

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

makeBot : (Move -> Move) -> Move -> Message
makeBot f m = GameFinished m (f m)

scoreGame : Score -> GameResult -> Score
scoreGame score result = 
    case result of
        PlayerOneWon -> { score | playerScore = score.playerScore + 1 }
        PlayerTwoWon -> { score | botScore = score.botScore + 1 }
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
                result = playGame player ai 
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

gameButton : Move -> Html Message
gameButton move = 
    div [class "button", onClick <| Play move] [i [class <| (moveToFaGlyph move ++ " fa-4x")] []]

view : Model -> Html Message
view model = 
    div [class "container"]
    [ stylesheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
    , stylesheet "https://www.w3schools.com/w3css/4/w3.css"
    , stylesheet "content/style.css"
    , div [class "w3-row board"]
        [ renderPlayerBox "Player" model.score.playerScore
        , renderBattleGround model.finishedGames
        , renderPlayerBox "Bot" model.score.botScore ] 
    , div [class "w3-row w3-content"] 
          [div [class "w3-col button", onClick Restart] [text "Restart"]]
    ]

renderBattleGround : List FinishedGame -> Html Message
renderBattleGround games = div [class "w3-third w3-row battleground"] 
             [ div [class "w3-col move-choose"] 
                    [ div [class "w3-row"] [gameButton Rock]
                    , div [class "w3-row"] [gameButton Scissors]
                    , div [class "w3-row"] [gameButton Paper]]
             , div [class "w3-rest game-archive"] <| List.map displayFinishedGame games
             ]

renderPlayerBox : String -> Int -> Html Message
renderPlayerBox name score =
    div [class "w3-third"]
        [ div [class "w3-row"] [text name]
        , div [class "w3-row"] [h1 [] [text <| toString score]]]

displayFinishedGame : FinishedGame -> Html Message
displayFinishedGame game = 
    h2 [class "finished-game"] 
        [ text "Player "
        , displayMove game.playerMove
        , text " vs "
        , displayMove game.botMove
        , text " Bot"
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
