module RestDojo.Main exposing (..)

import Html exposing (Html, text, a, div, span, img, article, header, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.App
import Http
import Task
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)


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
    , events : List Event
    }


initModel : ( Model, Cmd Msg )
initModel =
    { teams = []
    , events = []
    }
        ! [ initTeams ]


initTeams : Cmd Msg
initTeams =
    Task.perform TeamsLoadFailed TeamsLoadSucceed (API.getTeams)


initEvents : Cmd Msg
initEvents =
    Task.perform EventsLoadFailed EventsLoadSucceed (API.getEvents)



-- UPDATE --------------------------------------------------------------------------------------------------------------


type Msg
    = NotYet
    | TeamsLoadSucceed (List Team)
    | TeamsLoadFailed Http.Error
    | EventsLoadSucceed (List Event)
    | EventsLoadFailed Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamsLoadSucceed loadedTeams ->
            { model | teams = loadedTeams } ! [ initEvents ]

        EventsLoadSucceed loadedEvents ->
            { model | events = loadedEvents } ! []

        _ ->
            model ! []



-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , section []
            [ viewTeams model.teams
            , viewPoints model.teams
            , viewEvents model.events
            ]
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
        article [] <| h2Teams :: divTeams


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
        GameWonBy teamId ->
            viewEventGameWonBy teamId "Alpha"


viewEventGameWonBy : TeamId -> String -> Html Msg
viewEventGameWonBy teamId teamName =
    div [ class "rd-team" ]
        [ span [ class "rd-team-name" ]
            [ a [ href "" ] [ text "Game" ]
            , text <| " won by " ++ teamName
            ]
        , img
            [ src <| "https://robohash.org/" ++ teamName
            , class <| "rd-team-avatar rd-team-" ++ toString teamId
            ]
            []
        ]
