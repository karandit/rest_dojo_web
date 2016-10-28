module RestDojo.Main exposing (..)

import Html exposing (Html, text, div, span, img, article, header, h1, h2, section)
import Html.Attributes exposing (class, src)
import Html.App
import RestDojo.Types exposing (..)


-- MAIN ----------------------------------------------------------------------------------------------------------------


main : Program Never
main =
    Html.App.program
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL ---------------------------------------------------------------------------------------------------------------


type alias Model =
    { teams : List Team
    }


initModel : ( Model, Cmd Msg )
initModel =
    { teams =
        [ Team 3 "Charlie" "ver 1.0" 145
        , Team 1 "Alpha" "ver 0.1" 675
        , Team 5 "Echo" "ver blabla" 87
        , Team 4 "Delta" "ver 0.99" 99
        , Team 6 "Foxtrot" "ver 0.1.99" 67
        , Team 2 "Bravo" "ver 1.1" 543
        ]
    }
        ! []



-- UPDATE --------------------------------------------------------------------------------------------------------------


type Msg
    = NotYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , viewTeams model.teams
        ]


viewHeader : Html Msg
viewHeader =
    header []
        [ h1 [] [ text "Rest Dojo" ]
        ]


viewTeams : List Team -> Html Msg
viewTeams teams =
    let
        h2Teams =
            h2 [] [ text "Teams" ]

        divTeams =
            List.map viewTeam <| List.reverse <| List.sortBy .points teams
    in
        section []
            [ article []
                (h2Teams :: divTeams)
            ]


viewTeam : Team -> Html Msg
viewTeam team =
    div [ class "rd-team" ]
        [ img
            [ src <| "https://robohash.org/" ++ team.name
            , class <| "rd-team-avatar rd-team-" ++ toString team.id
            ]
            []
        , span [ class "rd-team-name" ] [ text team.name ]
        , span [ class "rd-team-descr" ] [ text team.descr ]
        , span [ class <| "rd-team-points rd-team-background-" ++ toString team.id ] [ text <| toString team.points ]
        ]
