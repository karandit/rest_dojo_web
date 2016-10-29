module RestDojo.API exposing (getTeams, getEvents)

import Dict exposing (Dict)
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


getEvents : List Team -> Task Error (List Event)
getEvents teams =
    let
        url =
            apiUrl ++ "/events"

        teamsByTeamId =
            Dict.fromList <| List.map (\team -> ( team.id, team )) teams
    in
        Http.get (eventsDecoder teamsByTeamId) url



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


eventsDecoder : Dict TeamId Team -> Decoder (List Event)
eventsDecoder teamsByTeamId =
    Json.list <| eventDecoder teamsByTeamId


eventDecoder : Dict TeamId Team -> Decoder Event
eventDecoder teamsByTeamId =
    ("gameWonBy" := Json.int)
        |> Json.map (\teamId -> GameWonBy <| Dict.get teamId teamsByTeamId)
