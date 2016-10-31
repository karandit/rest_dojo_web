module RestDojo.ViewDojo exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Dojo -> List (Html Msg)
view dojo =
    [ section []
        [ viewTeams dojo.teams
        , viewPoints dojo.teams
        , viewEvents dojo
        ]
    ]


viewTeams : List Team -> Html Msg
viewTeams teams =
    let
        h2Teams =
            h2 [] [ text "Teams" ]

        divTeams =
            List.map viewTeam <| List.reverse <| List.sortBy .points teams
    in
        article [] <| h2Teams :: divTeams


viewTeam : Team -> Html Msg
viewTeam team =
    div [ class "rd-team" ]
        [ img
            [ src <| avatar team.name
            , class <| "rd-team-avatar rd-team-" ++ toString team.id
            ]
            []
        , span [ class "rd-team-name" ] [ text team.name ]
        , span [ class "rd-team-descr" ] [ text team.descr ]
        , span [ class <| "rd-team-points rd-team-background-" ++ toString team.id ] [ text <| toString team.points ]
        ]


viewPoints : List Team -> Html Msg
viewPoints teams =
    article []
        [ h2 [] [ text "Points" ]
        , div [ class "rd-points" ] [ canvas [ id "chartPoints" ] [] ]
        ]


viewEvents : Dojo -> Html Msg
viewEvents dojo =
    let
        h2Events =
            h2 [] [ text "Events" ]

        divEvents =
            List.map (viewEvent dojo) dojo.events
    in
        article [] <| h2Events :: divEvents


viewEvent : Dojo -> Event -> Html Msg
viewEvent dojo event =
    case event of
        GameWonBy gameUrl winnerTeam ->
            viewEventGameWonBy dojo gameUrl winnerTeam


viewEventGameWonBy : Dojo -> GameUrl -> Maybe Team -> Html Msg
viewEventGameWonBy dojo gameUrl team =
    let
        label =
            case team of
                Just winnerTeam ->
                    " won by " ++ winnerTeam.name

                Nothing ->
                    " remained unsolved"

        avatarAttr =
            case team of
                Just winnerTeam ->
                    [ src <| avatar winnerTeam.name
                    , class <| "rd-team-avatar rd-team-" ++ toString winnerTeam.id
                    ]

                Nothing ->
                    [ class "rd-team-avatar" ]
    in
        div [ class "rd-team" ]
            [ span [ class "rd-team-name" ]
                [ button [ onClick <| SelectGame dojo gameUrl ] [ text "Game" ]
                , text label
                ]
            , img avatarAttr []
            ]
