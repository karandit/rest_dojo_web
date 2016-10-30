module RestDojo.Main exposing (..)

import Html exposing (Html, text, a, div, span, img, article, header, hr, h1, h2, section, canvas)
import Html.Attributes exposing (class, src, id, href)
import Html.App
import Http
import Task
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)


-- MAIN ----------------------------------------------------------------------------------------------------------------


type alias Flags =
    { baseUrl : String
    }


main : Program Flags
main =
    Html.App.programWithFlags
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL ---------------------------------------------------------------------------------------------------------------


type alias Model =
    { billboard : Billboard
    , route : Route
    , dojos : List Dojo
    , teams : List Team
    , events : List Event
    }


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    { billboard = Billboard "" "" ""
    , route = HomeRoute
    , dojos = []
    , teams = []
    , events = []
    }
        ! [ initBillboard flags.baseUrl ]


initBillboard : String -> Cmd Msg
initBillboard url =
    Task.perform ErrorOccured BillboardLoadSucceed (API.getBillboard url)


initDojos : String -> Cmd Msg
initDojos url =
    Task.perform ErrorOccured DojosLoadSucceed (API.getDojos url)


initTeams : String -> Cmd Msg
initTeams url =
    Task.perform ErrorOccured TeamsLoadSucceed (API.getTeams url)


initEvents : String -> List Team -> Cmd Msg
initEvents url teams =
    Task.perform ErrorOccured EventsLoadSucceed (API.getEvents url teams)



-- UPDATE --------------------------------------------------------------------------------------------------------------


type Msg
    = BillboardLoadSucceed Billboard
    | DojosLoadSucceed (List Dojo)
    | TeamsLoadSucceed (List Team)
    | EventsLoadSucceed (List Event)
    | ErrorOccured Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        BillboardLoadSucceed billboard ->
            { model | billboard = billboard } ! [ initTeams billboard.teamsUrl, initDojos billboard.dojosUrl ]

        DojosLoadSucceed loadedDojos ->
            { model | dojos = loadedDojos } ! []

        TeamsLoadSucceed loadedTeams ->
            { model | teams = loadedTeams } ! [ initEvents model.billboard.eventsUrl loadedTeams ]

        EventsLoadSucceed loadedEvents ->
            { model | events = loadedEvents } ! []

        _ ->
            model ! []



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
