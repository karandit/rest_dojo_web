module RestDojo.Main exposing (..)

import Html.App
import Http
import Task
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)
import RestDojo.View exposing (..)


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

        SelectDojo dojo ->
            { model | route = DojoRoute dojo } ! []

        _ ->
            model ! []
