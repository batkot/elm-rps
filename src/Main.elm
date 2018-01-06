module Main exposing (..)

import Html exposing (text, div, button, program, Html, node, i)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute, class)
import RPS exposing (Move(..), winsWith )
import RPS.AI exposing (constAI)

type Message = Restart
             | Play Move

type alias Model = 
    { playerScore : Int
    , computerScore : Int }

zeroScore : Model
zeroScore = Model 0 0

wins : Move -> Move -> Bool
wins = RPS.winsWith

decideWhoWon : Model -> Move -> Move -> Model
decideWhoWon model player ai =
    if player == ai then
        model
    else if player |> wins ai then
        { model | playerScore = model.playerScore + 1 }
    else 
        { model | computerScore = model.computerScore + 1 }


update : (Move -> Move)  -- AI Function
      -> Message            
      -> Model 
      -> (Model, Cmd Message)
update ai msg model = 
    case msg of 
        Restart -> (zeroScore, Cmd.none)
        Play move -> 
            let 
                aiMove = ai move
            in
               (decideWhoWon model move aiMove, Cmd.none)


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
        , div [class "score"] [ text <| toString model.playerScore, text ":" , text <| toString model.computerScore]
        , div [class "restart button", onClick Restart] [text "Restart"]
        ]
    ]

main : Program Never Model Message
main = program
        { init = (zeroScore, Cmd.none)
        , update = update <| constAI Rock
        , view = view
        , subscriptions = \_ -> Sub.none }
