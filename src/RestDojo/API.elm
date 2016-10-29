module RestDojo.API exposing (getTeams, getEvents, getBillboard)

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Http exposing (Error)
import Task exposing (Task)
import RestDojo.Types exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getBillboard : String -> Task Error Billboard
getBillboard url =
    Http.get billboardDecoder url


getTeams : String -> Task Error (List Team)
getTeams url =
    Http.get teamsDecoder url


getEvents : String -> List Team -> Task Error (List Event)
getEvents url teams =
    let
        teamsByTeamId =
            Dict.fromList <| List.map (\team -> ( team.id, team )) teams
    in
        Http.get (eventsDecoder teamsByTeamId) url



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


billboardDecoder : Decoder Billboard
billboardDecoder =
    Json.object2 Billboard
        ("teams" := Json.string)
        ("events" := Json.string)


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
