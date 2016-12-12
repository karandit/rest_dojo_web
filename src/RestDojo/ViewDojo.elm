module RestDojo.ViewDojo exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas, input, label)
import Html.Attributes exposing (class, src, id, href, attribute, for)
import Html.Events exposing (onClick)
import List.Extra
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Dojo -> Maybe User -> List (Html Msg)
view dojo loggedUser =
    [ section []
        [ viewTeams dojo loggedUser
        , viewPoints dojo.teams
        , viewEvents dojo
        ]
    ]
        ++ (viewDialog dojo)


viewDialog : Dojo -> List (Html Msg)
viewDialog dojo =
    case dojo.dialog of
        Just team ->
            [ viewDialogTeam dojo team ]

        Nothing ->
            []


viewDialogTeam : Dojo -> Team -> Html Msg
viewDialogTeam dojo team =
    div [ class "rd-modal rd-nodal--visible", attribute "role" "alert" ]
        [ div [ class "rd-modal__dialog" ]
            [ div [ class <| "rd-modal__header rd-team-background-" ++ (toString team.id) ]
                [ div [] [ teamImg team ]
                , div [ class "rd-modal__header-title" ] [ text team.name ]
                ]
            , button [ class "rd-modal__action", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            , a [ class "cd-popup-close img-replace", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            ]
        ]


teamImg team =
    let
        avatarAttr =
            [ src <| avatar team.name
            , class <| "rd-team-avatar rd-team-" ++ toString team.id
            ]
    in
        img avatarAttr []


viewTeams : Dojo -> Maybe User -> Html Msg
viewTeams dojo loggedUser =
    let
        userAndTeam =
            case loggedUser of
                Just user ->
                    let
                        userTeam =
                            List.Extra.find (\team -> team.captain == user.nickname) dojo.teams
                    in
                        Just ( user, userTeam )

                Nothing ->
                    Nothing

        h2Teams =
            h2 [] [ text "Teams" ]

        divTeams =
            List.map (viewTeam dojo userAndTeam) <| List.reverse <| List.sortBy .points dojo.teams
    in
        article [] <| h2Teams :: divTeams


viewTeam : Dojo -> Maybe ( User, Maybe Team ) -> Team -> Html Msg
viewTeam dojo userAndTeam team =
    let
        action =
            case dojo.state of
                Upcoming ->
                    span [] []

                Past ->
                    span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowTeamDialog dojo team ] [ text "View Team" ]

                Running ->
                    case userAndTeam of
                        Just ( user, maybeUserTeam ) ->
                            case maybeUserTeam of
                                Just userTeam ->
                                    if team.id == userTeam.id then
                                        span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowTeamDialog dojo team ] [ text "My Team" ]
                                    else
                                        span [] []

                                Nothing ->
                                    span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowTeamDialog dojo team ] [ text "Join Team" ]

                        Nothing ->
                            span [] []
    in
        div [ class "rd-team" ]
            [ teamImg team
            , span [ class "rd-team-name" ] [ text team.name ]
            , span [ class "rd-team-descr" ] [ text team.descr ]
            , span [ class <| "rd-team-points rd-team-background-" ++ toString team.id ] [ text <| toString team.points ]
            , action
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
                [ a [ href "#", onClick <| SelectGame dojo gameUrl ] [ text "Game" ]
                , text label
                ]
            , img avatarAttr []
            ]
