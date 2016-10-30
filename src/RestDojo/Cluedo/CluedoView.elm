module RestDojo.Cluedo.CluedoView exposing (view)

import Html exposing (Html, text, div, hr)
import RestDojo.Types exposing (..)


view : Game -> List (Html Msg)
view game =
    [ div [] [ text <| toString game.secret.person ]
    , div [] [ text <| toString game.secret.weapon ]
    , div [] [ text <| toString game.secret.location ]
    , hr [] []
    ]
        ++ (List.map viewBot game.bots)


viewBot : Bot -> Html Msg
viewBot bot =
    div []
        [ div [] <| List.map viewCard bot.persons
        , div [] <| List.map viewCard bot.locations
        , div [] <| List.map viewCard bot.weapons
        , hr [] []
        ]


viewCard : a -> Html Msg
viewCard card =
    text <| toString card
