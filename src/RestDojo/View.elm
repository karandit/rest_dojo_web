module RestDojo.View exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [] <|
        (viewHeader model)
            ++ (viewContent model)


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ viewBreadcrumbs model ]


viewBreadcrumbs : Model -> Html Msg
viewBreadcrumbs model =
    let
        breadcrumbs =
            case model.route of
                HomeRoute ->
                    [ text "Rest Dojo" ]

                DojoRoute dojoId ->
                    let
                        dojoLabel =
                            model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head |> Maybe.map .label |> Maybe.withDefault "Unknow dojo"
                    in
                        [ text "Rest Dojo", text " \\ ", text dojoLabel ]
    in
        header []
            [ h1 [] breadcrumbs
            ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model.route of
        HomeRoute ->
            viewHomePage model.dojos

        DojoRoute dojoId ->
            let
                foundDojo =
                    model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head
            in
                case foundDojo of
                    Just dojo ->
                        viewDojoPage dojo

                    Nothing ->
                        viewNotFound


viewHomePage : List Dojo -> List (Html Msg)
viewHomePage dojos =
    [ section [] [ viewDojos "Running Dojos" "running" dojos ]
    , div [] []
    , section [] [ viewDojos "Past Dojos" "past" dojos ]
    ]


viewDojoPage : Dojo -> List (Html Msg)
viewDojoPage dojo =
    [ section []
        [ viewTeams dojo.teams
        , viewPoints dojo.teams
        , viewEvents dojo.events
        ]
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
            [ src <| avatar "aa", class "rd-team-avatar" ]
            []
        , button
            [ class "rd-team-name", onClick (SelectDojo dojo) ]
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


viewNotFound : List (Html Msg)
viewNotFound =
    [ div [] [ text "Not Found" ] ]


avatar : String -> String
avatar name =
    "http://robohash.herokuapp.com/" ++ name ++ ".png"
