port module RestDojo.Main exposing (..)

import Html
import Http
import Task
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)
import RestDojo.View exposing (..)
import RestDojo.Chartjs exposing (..)
import List.Extra


-- MAIN ----------------------------------------------------------------------------------------------------------------


type alias Flags =
    { baseUrl : String
    }


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> authentications LoggedIn
        }


port chart : ChartInput -> Cmd msg


port auth0 : String -> Cmd msg


port authentications : (User -> msg) -> Sub msg


mapToChartInput : PointHistory -> ChartInput
mapToChartInput pointHistory =
    { labels = pointHistory.games
    , datasets =
        pointHistory.teams
            |> List.Extra.zip [ "#7e5ae2", "#e25abc", "#e25a77", "#7e9ce2", "#f78764", "#1784c7" ]
            |> List.map (\( color, teamPoints ) -> { label = teamPoints.teamName, data = teamPoints.data, borderColor = color })
    }



-- MODEL ---------------------------------------------------------------------------------------------------------------


initModel : Flags -> ( Model, Cmd Msg )
initModel flags =
    { billboard = Billboard ""
    , route = HomeRoute
    , dojos = []
    , user = Nothing
    }
        ! [ initBillboard flags.baseUrl ]


initBillboard : String -> Cmd Msg
initBillboard url =
    Http.send LoadBillboard (API.getBillboard url)


initDojos : String -> Cmd Msg
initDojos url =
    Http.send LoadDojos (API.getDojos url)


initTeams : Dojo -> Cmd Msg
initTeams dojo =
    Http.send (LoadTeams dojo) (API.getTeams dojo.teamsUrl)


loadPointHistory : Dojo -> Cmd Msg
loadPointHistory dojo =
    Http.send LoadPointHistory (API.getPointHistory dojo.pointHistoryUrl)


initEvents : Dojo -> List Team -> Cmd Msg
initEvents dojo teams =
    Http.send (LoadEvents dojo) (API.getEvents dojo.eventsUrl teams)


initGame : Dojo -> GameUrl -> Cmd Msg
initGame dojo gameUrl =
    Http.send (LoadGame dojo) (API.getGame gameUrl)



-- UPDATE --------------------------------------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        LoadBillboard (Ok billboard) ->
            { model | billboard = billboard } ! [ initDojos billboard.dojosUrl ]

        LoadDojos (Ok loadedDojos) ->
            { model | dojos = loadedDojos } ! []

        SelectDojo dojo ->
            { model | route = DojoRoute dojo.id } ! [ initTeams dojo, loadPointHistory dojo ]

        SelectGame dojo gameUrl ->
            model ! [ initGame dojo gameUrl ]

        LoadPointHistory (Ok pointHistory) ->
            model ! [ chart <| mapToChartInput pointHistory ]

        LoadGame dojo (Ok game) ->
            { model | route = GameRoute dojo.id game } ! []

        LoadTeams oldDojo (Ok loadedTeams) ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = loadedTeams }) model.dojos } ! [ initEvents oldDojo loadedTeams ]

        LoginPushed ->
            model ! [ auth0 "test" ]

        LoggedIn loggeduser ->
            { model | user = Just loggeduser } ! []

        ShowTeamDialog oldDojo team ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Just team }) model.dojos } ! []

        CloseTeamDialog oldDojo ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Nothing }) model.dojos } ! []

        LoadEvents oldDojo (Ok loadedEvents) ->
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
