module RestDojo.API exposing (getTeams, getEvents)

import Json.Decode as Json exposing (..)
import Http exposing (Error)
import Task exposing (Task)
import RestDojo.Types exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


apiUrl : String
apiUrl =
    "/rest_dojo_web/api"


getTeams : Task Error (List Team)
getTeams =
    let
        url =
            apiUrl ++ "/teams.json"
    in
        Http.get teamsDecoder url


getEvents : Task Error (List Event)
getEvents =
    let
        url =
            apiUrl ++ "/events"
    in
        Http.get eventsDecoder url



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


teamsDecoder : Decoder (List Team)
teamsDecoder =
    Json.list teamDecoder


teamDecoder : Decoder Team
teamDecoder =
    Json.object4 Team
        ("id" := Json.int)
        ("name" := Json.string)
        ("descr" := Json.string)
        ("points" := Json.int)


eventsDecoder : Decoder (List Event)
eventsDecoder =
    Json.list eventDecoder


eventDecoder : Decoder Event
eventDecoder =
    Json.object1 GameWonBy
        ("gameWonBy" := Json.int)
