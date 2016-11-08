module RestDojo.View exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.ViewHome as ViewHome
import RestDojo.ViewDojo as ViewDojo
import RestDojo.Cluedo.CluedoView as ViewCluedo


-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div [] <|
        (viewHeader model)
            ++ (viewContent model)


viewHeader : Model -> List (Html Msg)
viewHeader model =
    [ viewBreadcrumbs model
    , viewLogin model
    ]


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

                GameRoute dojoId game ->
                    let
                        foundDojo =
                            model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head

                        dojoLabel =
                            foundDojo |> Maybe.map .label |> Maybe.withDefault "Unknow dojo"
                    in
                        [ text "Rest Dojo", text " \\ ", text dojoLabel, text " \\ ", text <| toString game.id ]
    in
        header []
            [ h1 [] breadcrumbs
            ]


viewLogin : Model -> Html Msg
viewLogin model =
    case model.user of
        Just loggedUser ->
            div [] [ text loggedUser.name ]

        Nothing ->
            button
                [ onClick LoginPushed ]
                [ text "Log in with Github" ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model.route of
        HomeRoute ->
            ViewHome.view model.dojos

        DojoRoute dojoId ->
            let
                foundDojo =
                    model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head
            in
                case foundDojo of
                    Just dojo ->
                        ViewDojo.view dojo

                    Nothing ->
                        viewNotFound

        GameRoute dojoId game ->
            let
                foundDojo =
                    model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head
            in
                case foundDojo of
                    Just dojo ->
                        ViewCluedo.view dojo game

                    Nothing ->
                        viewNotFound


viewNotFound : List (Html Msg)
viewNotFound =
    [ div [] [ text "Not Found" ] ]
