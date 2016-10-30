module RestDojo.Cluedo.CluedoView exposing (view)

import Html exposing (Html, text, div)
import RestDojo.Types exposing (..)


view : Game -> List (Html Msg)
view game =
    [ div [] [ text <| toString game.secret.person ]
    , div [] [ text <| toString game.secret.weapon ]
    , div [] [ text <| toString game.secret.location ]
    ]
