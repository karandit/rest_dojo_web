module RestDojo.ViewDojo exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas, input, label)
import Html.Attributes exposing (class, src, id, href, attribute, for, type')
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Dojo -> List (Html Msg)
view dojo =
    [ section []
        [ viewTeams dojo dojo.teams
          --TODO dejo.teams is redundant
        , viewPoints dojo.teams
        , viewEvents dojo
        ]
    ]
        ++ (if dojo.dialog then
                [ viewDialog dojo ]
            else
                []
           )


viewDialog : Dojo -> Html Msg
viewDialog dojo =
    div [ class "rd-modal rd-nodal--visible", attribute "role" "alert" ]
        [ div [ class "rd-modal__dialog" ]
            [ div [ class "rd-modal__header rd-team-background-1" ]
                []
            , div [ class "input input--hoshi" ]
                [ input [ class "input__field input__field--hoshi", id "input-4", type' "text" ]
                    []
                , label [ class "input__label input__label--hoshi input__label--hoshi-color-2", for "input-4" ]
                    [ span [ class "input__label-content input__label-content--hoshi" ]
                        [ text "Team name" ]
                    ]
                ]
            , button [ class "rd-modal__action" ]
                [ text "Close" ]
            , a [ class "cd-popup-close img-replace", onClick (CloseTeamDialog dojo) ]
                [ text "Close" ]
            ]
        ]


viewTeams : Dojo -> List Team -> Html Msg
viewTeams dojo teams =
    let
        h2Teams =
            h2 [] [ text "Teams" ]

        divTeams =
            List.map (viewTeam dojo) <| List.reverse <| List.sortBy .points teams
    in
        article [] <| h2Teams :: divTeams


viewTeam : Dojo -> Team -> Html Msg
viewTeam dojo team =
    div [ class "rd-team" ]
        [ img
            [ src <| avatar team.name
            , class <| "rd-team-avatar rd-team-" ++ toString team.id
            ]
            []
        , span [ class "rd-team-name" ] [ text team.name ]
        , span [ class "rd-team-descr" ] [ text team.descr ]
        , span [ class "rd-team-action", onClick <| ShowTeamDialog dojo team ] [ text "Team" ]
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
