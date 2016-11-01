port module RestDojo.Main exposing (..)

import Html.App
import Http
import Task
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)
import RestDojo.View exposing (..)
import RestDojo.Chartjs exposing (..)


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


port chart : ChartInput -> Cmd msg


mapToChartInput : PointHistory -> ChartInput
mapToChartInput pointHistory =
    { labels = pointHistory.games
    , datasets = List.map (\teamPoints -> { label = teamPoints.teamName, data = teamPoints.data, borderColor = "#7e5ae2" }) pointHistory.teams
    }



-- , borderColor = "#7e5ae2"
-- , borderColor = "#e25a77"
-- , borderColor = "#7e9ce2"
-- , borderColor = "#F78764"
-- , borderColor = "#1784c7"
-- MODEL ---------------------------------------------------------------------------------------------------------------


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    { billboard = Billboard ""
    , route = HomeRoute
    , dojos = []
    }
        ! [ initBillboard flags.baseUrl ]


initBillboard : String -> Cmd Msg
initBillboard url =
    Task.perform ErrorOccured BillboardLoadSucceed (API.getBillboard url)


initDojos : String -> Cmd Msg
initDojos url =
    Task.perform ErrorOccured DojosLoadSucceed (API.getDojos url)


initTeams : Dojo -> Cmd Msg
initTeams dojo =
    Task.perform ErrorOccured (TeamsLoadSucceed dojo) (API.getTeams dojo.teamsUrl)


loadPointHistory : Dojo -> Cmd Msg
loadPointHistory dojo =
    Task.perform ErrorOccured PointHistoryLoadSucceed (API.getPointHistory dojo.pointHistoryUrl)


initEvents : Dojo -> List Team -> Cmd Msg
initEvents dojo teams =
    Task.perform ErrorOccured (EventsLoadSucceed dojo) (API.getEvents dojo.eventsUrl teams)


initGame : Dojo -> GameUrl -> Cmd Msg
initGame dojo gameUrl =
    Task.perform ErrorOccured (GameLoadSucceed dojo) (API.getGame gameUrl)



-- UPDATE --------------------------------------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        BillboardLoadSucceed billboard ->
            { model | billboard = billboard } ! [ initDojos billboard.dojosUrl ]

        DojosLoadSucceed loadedDojos ->
            { model | dojos = loadedDojos } ! []

        SelectDojo dojo ->
            { model | route = DojoRoute dojo.id } ! [ initTeams dojo, loadPointHistory dojo ]

        SelectGame dojo gameUrl ->
            model ! [ initGame dojo gameUrl ]

        PointHistoryLoadSucceed pointHistory ->
            model ! [ chart <| mapToChartInput pointHistory ]

        GameLoadSucceed dojo game ->
            { model | route = GameRoute dojo.id game } ! []

        TeamsLoadSucceed oldDojo loadedTeams ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = loadedTeams }) model.dojos } ! [ initEvents oldDojo loadedTeams ]

        EventsLoadSucceed oldDojo loadedEvents ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | events = loadedEvents }) model.dojos } ! []

        _ ->
            model ! []


updateDojo dojoId updater dojos =
    List.map
        (\dojo ->
            if dojo.id == dojoId then
                updater dojo
            else
                dojo
        )
        dojos
