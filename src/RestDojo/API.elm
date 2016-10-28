module RestDojo.API exposing (getTeams)

import Json.Decode as Json exposing (..)
import Http exposing (Error)
import Task exposing (Task)
import RestDojo.Types exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


apiUrl : String
apiUrl =
    "/api"


getTeams : Task Error (List Team)
getTeams =
    let
        url =
            apiUrl ++ "/teams.json"
    in
        Http.get teamsDecoder url



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
