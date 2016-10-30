module RestDojo.View exposing (view)

import Html exposing (Html, text, a, button, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.Events exposing (onClick)
import RestDojo.Types exposing (..)
import RestDojo.ViewHome as ViewHome
import RestDojo.ViewDojo as ViewDojo


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

                GameRoute dojoId gameId ->
                    let
                        foundDojo =
                            model.dojos |> List.filter (\dojo -> dojo.id == dojoId) |> List.head

                        dojoLabel =
                            foundDojo |> Maybe.map .label |> Maybe.withDefault "Unknow dojo"
                    in
                        [ text "Rest Dojo", text " \\ ", text dojoLabel, text " \\ ", text <| toString gameId ]
    in
        header []
            [ h1 [] breadcrumbs
            ]


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

        GameRoute dojoId gameId ->
            viewNotFound


viewNotFound : List (Html Msg)
viewNotFound =
    [ div [] [ text "Not Found" ] ]
