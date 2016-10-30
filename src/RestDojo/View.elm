module RestDojo.View exposing (view)

import Html exposing (Html, text, a, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import RestDojo.Types exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [] <|
        (viewHeader model)
            ++ (viewContent model)


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ viewBreadcrumbs model.route ]


viewBreadcrumbs : Route -> Html Msg
viewBreadcrumbs route =
    header []
        [ h1 [] [ text "Rest Dojo" ]
        ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model.route of
        HomeRoute ->
            viewHomePage model.dojos

        DojoRoute dojo ->
            viewDojoPage model dojo


viewHomePage : List Dojo -> List (Html Msg)
viewHomePage dojos =
    [ section [] [ viewDojos "Running Dojos" "running" dojos ]
    , div [] []
    , section [] [ viewDojos "Past Dojos" "past" dojos ]
    ]


viewDojoPage : Model -> Dojo -> List (Html Msg)
viewDojoPage model dojo =
    [ section []
        [ viewTeams model.teams
        , viewPoints model.teams
        , viewEvents model.events
        ]
    ]


viewDojos : String -> String -> List Dojo -> Html Msg
viewDojos label state dojos =
    let
        h2Dojos =
            h2 [] [ text label ]

        divDojos =
            List.map viewDojo <| List.filter (\dojo -> dojo.state == state) dojos
    in
        article [] <| h2Dojos :: divDojos


viewDojo : Dojo -> Html Msg
viewDojo dojo =
    div [ class "rd-team" ]
        [ img
            [ src <| avatar "aa", class "rd-team-avatar" ]
            []
        , span
            [ class "rd-team-name" ]
            [ text dojo.label ]
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


viewEvents : List Event -> Html Msg
viewEvents events =
    let
        h2Events =
            h2 [] [ text "Events" ]

        divEvents =
            List.map viewEvent events
    in
        article [] <| h2Events :: divEvents


viewEvent : Event -> Html Msg
viewEvent event =
    case event of
        GameWonBy winnerTeam ->
            viewEventGameWonBy winnerTeam


viewEventGameWonBy : Maybe Team -> Html Msg
viewEventGameWonBy team =
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
                [ a [ href "" ] [ text "Game" ]
                , text label
                ]
            , img avatarAttr []
            ]


avatar : String -> String
avatar name =
    "http://robohash.herokuapp.com/" ++ name ++ ".png"
