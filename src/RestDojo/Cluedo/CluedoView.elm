module RestDojo.Cluedo.CluedoView exposing (view)

import Html exposing (Html, text, div, hr, img)
import Html.Attributes exposing (title, src, width, height)
import RestDojo.Types exposing (..)


view : Game -> List (Html Msg)
view game =
    [ div [] <| List.map viewCard [ toString game.secret.person, toString game.secret.weapon, toString game.secret.location ]
    , hr [] []
    ]
        ++ (List.map viewBot game.bots)


viewBot : Bot -> Html Msg
viewBot bot =
    let
        allBotCards =
            (List.map toString bot.persons) ++ (List.map toString bot.locations) ++ (List.map toString bot.weapons)
    in
        div [] <| List.map viewCardSmall allBotCards


viewCard : String -> Html Msg
viewCard =
    viewCardWithSize 144 180


viewCardSmall : String -> Html Msg
viewCardSmall =
    viewCardWithSize 80 100


viewCardWithSize : Int -> Int -> String -> Html Msg
viewCardWithSize w h cardName =
    img [ src <| "img/cards/" ++ cardName ++ ".png", width w, height h, title cardName ] []
