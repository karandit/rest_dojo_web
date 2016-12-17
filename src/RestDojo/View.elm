module RestDojo.View exposing (view)

import Html exposing (Html, text, a, button, div, span, p, strong, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.ViewHome as ViewHome
import RestDojo.ViewDojo as ViewDojo
import RestDojo.Cluedo.CluedoView as ViewCluedo
import RestDojo.Minesweeper.MinesweeperView as ViewMinesweeper


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , div [] <| viewContent model
        , viewAlerts model.alerts
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ viewBreadcrumbs model
        , viewLogin model
        ]


viewBreadcrumbs : Model -> Html Msg
viewBreadcrumbs model =
    let
        breadcrumbs =
            case model.route of
                NotFoundRoute ->
                    [ text "Rest Dojo" ]

                HomeRoute ->
                    [ text "Rest Dojo" ]

                DojoRoute dojoId ->
                    let
                        dojoLabel =
                            model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head |> Maybe.map .label |> Maybe.withDefault "Unknow dojo"
                    in
                        [ a [ href "#", onClick SelectHome ] [ text "Rest Dojo" ]
                        , text " / "
                        , text dojoLabel
                        ]

                GameRoute dojoId game ->
                    let
                        maybeFoundDojo =
                            model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head

                        dojoLink =
                            case maybeFoundDojo of
                                Just foundDojo ->
                                    a [ href <| "#dojos/" ++ (toString foundDojo.id), onClick (SelectDojo foundDojo) ] [ text foundDojo.label ]

                                Nothing ->
                                    text "Unknow dojo"
                    in
                        [ a [ href "#", onClick SelectHome ] [ text "Rest Dojo" ]
                        , text " / "
                        , dojoLink
                        , text " / "
                        , text <| getGameLabel game
                        ]
    in
        h1 [] breadcrumbs


getGameLabel : Game -> String
getGameLabel game =
    case game of
        Cluedo cludoGame ->
            toString cludoGame.id

        Minesweeper msGame ->
            "xxx"


viewLogin : Model -> Html Msg
viewLogin model =
    case model.user of
        Just loggedUser ->
            img [ class "rd-login rd-avatar", src loggedUser.picture ] []

        Nothing ->
            span [ class "rd-login rd__button", onClick LoginPushed ] [ text "Log in with Github" ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model.route of
        NotFoundRoute ->
            viewNotFound

        HomeRoute ->
            ViewHome.view model.dojos

        DojoRoute dojoId ->
            let
                foundDojo =
                    model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head
            in
                case foundDojo of
                    Just dojo ->
                        ViewDojo.view dojo model.user

                    Nothing ->
                        viewNotFound

        GameRoute dojoId game ->
            let
                foundDojo =
                    model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head
            in
                case foundDojo of
                    Just dojo ->
                        case game of
                            Cluedo cluedoGame ->
                                ViewCluedo.view dojo cluedoGame

                            Minesweeper minesweeperGame ->
                                ViewMinesweeper.view dojo minesweeperGame

                    Nothing ->
                        viewNotFound


viewNotFound : List (Html Msg)
viewNotFound =
    [ div [] [ text "Not Found" ] ]


viewAlerts : List Alert -> Html Msg
viewAlerts alerts =
    div [ class "ns-box-cont" ] <|
        List.map viewAlert alerts


viewAlert : Alert -> Html Msg
viewAlert alert =
    div [ class "ns-box ns-growl ns-effect-jelly ns-type-warning ns-show" ]
        [ div [ class "ns-box-inner" ]
            [ p [] [ text alert.message ]
            ]
        , span [ class "ns-close", onClick <| CloseAlert alert.id ] []
        ]
