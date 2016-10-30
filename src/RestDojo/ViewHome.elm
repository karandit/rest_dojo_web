module RestDojo.ViewHome exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : List Dojo -> List (Html Msg)
view dojos =
    [ section [] [ viewDojos "Running Dojos" "running" dojos ]
    , div [] []
    , section [] [ viewDojos "Past Dojos" "past" dojos ]
    ]


viewDojos : String -> String -> List Dojo -> Html Msg
viewDojos label state dojos =
    let
        h2Dojos =
            h2 [] [ text label ]

        divDojos =
            dojos
                |> List.filter (\dojo -> dojo.state == state)
                |> List.map viewDojo
    in
        article [] <| h2Dojos :: divDojos


viewDojo : Dojo -> Html Msg
viewDojo dojo =
    div [ class "rd-team" ]
        [ img
            --TODO : use a proper icon instead of avatars
            [ src <| avatar "aa", class "rd-team-avatar" ]
            []
        , button
            [ class "rd-team-name", onClick (SelectDojo dojo) ]
            [ text dojo.label ]
        ]


avatar : String -> String
avatar name =
    "http://robohash.herokuapp.com/" ++ name ++ ".png"
