module RestDojo.ViewDojo exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas, input, label)
import Html.Attributes exposing (class, src, id, href, attribute, for, type_)
import Html.Events exposing (onClick, onInput)
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
        Just (EditTeamDialog team) ->
            [ viewShowTeamDialog dojo team ]

        Just (JoinTeamDialog team) ->
            [ viewJoinTeamDialog dojo team ]

        Just (CreateTeamDialog teamName) ->
            [ viewCreateTeamDialog dojo teamName ]

        Nothing ->
            []


testUsers : List User
testUsers =
    [ { name = "bali182", fullname = "Balázs Édes", picture = "https://avatars.githubusercontent.com/u/3879181?v=3" }
    , { name = "zeldan", fullname = "Dániel Zelei", picture = "https://avatars.githubusercontent.com/u/8355979?v=3" }
    , { name = "szokebarnabas", fullname = "Barnabás Szőke", picture = "https://avatars.githubusercontent.com/u/5434245?v=3" }
    ]


viewShowTeamDialog : Dojo -> Team -> Html Msg
viewShowTeamDialog dojo team =
    div [ class "rd-modal rd-nodal--visible", attribute "role" "alert" ]
        [ div [ class "rd-modal__dialog" ]
            [ div [ class <| "rd-modal__header rd-team-background-" ++ teamNumber team ]
                [ div [] [ teamImg team ]
                , div [ class "rd-modal__header-title" ] [ text team.name ]
                ]
            , div [] <| List.map viewTeamMember testUsers
            , button [ class "rd-modal__action", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            , a [ class "cd-popup-close img-replace", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            ]
        ]


viewTeamMember : User -> Html Msg
viewTeamMember teamMember =
    div []
        [ img [ class "rd-avatar", src teamMember.picture ] []
        , span [] [ text teamMember.fullname ]
        , span [ class "rd__button rd__button--small" ] [ text "Yes" ]
        , span [] [ text " " ]
        , span [ class "rd__button rd__button--small" ] [ text " No " ]
        ]


viewJoinTeamDialog : Dojo -> Team -> Html Msg
viewJoinTeamDialog dojo team =
    div [ class "rd-modal rd-nodal--visible", attribute "role" "alert" ]
        [ div [ class "rd-modal__dialog" ]
            [ div [ class <| "rd-modal__header rd-team-background-" ++ teamNumber team ]
                [ div [] [ teamImg team ]
                , div [ class "rd-modal__header-title" ] [ text team.name ]
                ]
            , button [ class "rd-modal__action", onClick (JoinTeam dojo team) ] [ text "Join team" ]
            , a [ class "cd-popup-close img-replace", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            ]
        ]


viewCreateTeamDialog : Dojo -> String -> Html Msg
viewCreateTeamDialog dojo teamName =
    div [ class "rd-modal rd-nodal--visible", attribute "role" "alert" ]
        [ div [ class "rd-modal__dialog" ]
            [ div [ class <| "rd-modal__header rd-default-background" ]
                [ div [ class "rd-modal__header-title" ] [ text "New team" ]
                ]
            , div [ class "input input--hoshi" ]
                [ input [ class "input__field input__field--hoshi", id "input-team-name", onInput (EditTeamNameInDialog dojo), type_ "text" ]
                    []
                , label [ class "input__label input__label--hoshi input__label--hoshi-color-2", for "input-team-name" ]
                    [ span [ class "input__label-content input__label-content--hoshi" ]
                        [ text "Team name" ]
                    ]
                ]
            , button [ class "rd-modal__action", onClick (CreateTeam dojo teamName) ] [ text "Create new team" ]
            , a [ class "cd-popup-close img-replace", onClick (CloseTeamDialog dojo) ] [ text "Close" ]
            ]
        ]


teamNumber : Team -> String
teamNumber team =
    let
        mod6 =
            team.id % 6

        m1to6 =
            if mod6 == 0 then
                6
            else
                mod6
    in
        toString m1to6


teamImg team =
    let
        avatarAttr =
            [ src <| avatar team.name
            , class <| "rd-team-avatar rd-team-" ++ teamNumber team
            ]
    in
        img avatarAttr []


viewTeams : Dojo -> Maybe User -> Html Msg
viewTeams dojo loggedUser =
    let
        isMyTeam user team =
            if (team.captain.name == user.name) then
                True
            else
                List.Extra.find (\teamMember -> teamMember.name == user.name) team.members
                    |> Maybe.map (\_ -> True)
                    |> Maybe.withDefault False

        userAndTeam =
            case loggedUser of
                Just user ->
                    let
                        userTeam =
                            List.Extra.find (isMyTeam user) dojo.teams
                    in
                        Just ( user, userTeam )

                Nothing ->
                    Nothing

        divTeams =
            List.map (viewTeam dojo userAndTeam) <| List.reverse <| List.sortBy .points dojo.teams

        headAndTeams =
            [ h2 [] [ text "Teams" ] ] ++ divTeams

        divs =
            if needsAddNewTeam dojo userAndTeam then
                headAndTeams ++ [ div [ class "rd__button", onClick <| ShowCreateTeamDialog dojo ] [ text "New team" ] ]
            else
                headAndTeams
    in
        article [] divs


needsAddNewTeam : Dojo -> Maybe ( User, Maybe Team ) -> Bool
needsAddNewTeam dojo userAndTeam =
    if List.length dojo.teams > 5 then
        False
    else
        case dojo.state of
            Upcoming ->
                False

            Past ->
                False

            Running ->
                case userAndTeam of
                    Just ( user, maybeUserTeam ) ->
                        case maybeUserTeam of
                            Just userTeam ->
                                False

                            Nothing ->
                                True

                    Nothing ->
                        False


viewTeam : Dojo -> Maybe ( User, Maybe Team ) -> Team -> Html Msg
viewTeam dojo userAndTeam team =
    let
        action =
            case dojo.state of
                Upcoming ->
                    span [] []

                Past ->
                    span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowEditTeamDialog dojo team ] [ text "View Team" ]

                Running ->
                    case userAndTeam of
                        Just ( user, maybeUserTeam ) ->
                            case maybeUserTeam of
                                Just userTeam ->
                                    if team.id == userTeam.id then
                                        span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowEditTeamDialog dojo team ] [ text "My Team" ]
                                    else
                                        span [] []

                                Nothing ->
                                    span [ class "rd-team-action rd__button rd__button--small", onClick <| ShowJoinTeamDialog dojo team ] [ text "Join Team" ]

                        Nothing ->
                            span [] []
    in
        div [ class "rd-team" ]
            [ teamImg team
            , span [ class "rd-team-name" ] [ text team.name ]
            , span [ class "rd-team-descr" ] [ text team.descr ]
            , span [ class <| "rd-team-points rd-team-background-" ++ teamNumber team ] [ text <| toString team.points ]
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
                    , class <| "rd-team-avatar rd-team-" ++ teamNumber winnerTeam
                    ]

                Nothing ->
                    [ class "rd-team-avatar" ]
    in
        div [ class "rd-team" ]
            [ span [ class "rd-team-name" ]
                [ a [ href <| "#dojos/" ++ (toString dojo.id) ++ "/games/999", onClick <| SelectGame dojo gameUrl ] [ text "Game" ]
                , text label
                ]
            , img avatarAttr []
            ]
