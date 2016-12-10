module RestDojo.ViewHome exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, h3, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : List Dojo -> List (Html Msg)
view dojos =
    [ viewDojos "Running Dojos" Running dojos
    , viewDojos "Past Dojos" Past dojos
    ]


viewDojos : String -> DojoState -> List Dojo -> Html Msg
viewDojos label state dojos =
    let
        h2Dojos =
            h2 [] [ text label ]

        divDojos =
            dojos
                |> List.filter (\dojo -> dojo.state == state)
                |> List.map viewDojo
    in
        div [ class "rd-dojos" ] <|
            h2Dojos
                :: divDojos


viewDojo : Dojo -> Html Msg
viewDojo dojo =
    div [ class "rd-dojo", onClick (SelectDojo dojo) ]
        [ h3 [] [ text dojo.label ]
        , div [ class "rd-dojo-descr" ] [ text <| toString dojo.dojoType ]
        ]
