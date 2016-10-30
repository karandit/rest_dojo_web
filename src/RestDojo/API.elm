module RestDojo.API exposing (getTeams, getEvents, getBillboard, getDojos, getGame)

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Http exposing (Error)
import Task exposing (Task)
import RestDojo.Types exposing (..)
import RestDojo.Cluedo.CluedoTypes exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getBillboard : String -> Task Error Billboard
getBillboard url =
    Http.get billboardDecoder url


getDojos : String -> Task Error (List Dojo)
getDojos url =
    Http.get dojosDecoder url


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


getGame : GameUrl -> Task Error Game
getGame url =
    Http.get gameDecoder url



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


billboardDecoder : Decoder Billboard
billboardDecoder =
    Json.object1 Billboard
        ("dojos" := Json.string)


dojosDecoder : Decoder (List Dojo)
dojosDecoder =
    Json.list <|
        Json.object5 (Dojo [] [])
            ("id" := Json.int)
            ("label" := Json.string)
            ("state" := dojoStateDecoder)
            ("teamsUrl" := Json.string)
            ("eventsUrl" := Json.string)


dojoStateDecoder : Json.Decoder DojoState
dojoStateDecoder =
    let
        decodeToType string =
            case string of
                "running" ->
                    Result.Ok Running

                "past" ->
                    Result.Ok Past

                "upcoming" ->
                    Result.Ok Upcoming

                _ ->
                    Result.Err ("Not valid pattern for decoder to DojoState. Pattern: " ++ (toString string))
    in
        Json.customDecoder Json.string decodeToType


teamsDecoder : Decoder (List Team)
teamsDecoder =
    Json.list <|
        Json.object4 Team
            ("id" := Json.int)
            ("name" := Json.string)
            ("descr" := Json.string)
            ("points" := Json.int)


eventsDecoder : Dict TeamId Team -> Decoder (List Event)
eventsDecoder teamsByTeamId =
    Json.list <|
        Json.map (\( gameUrl, teamId ) -> GameWonBy gameUrl (Dict.get teamId teamsByTeamId)) <|
            Json.object2 (,)
                ("gameUrl" := Json.string)
                ("gameWonBy" := Json.int)


gameDecoder : Decoder Game
gameDecoder =
    Json.object3 Game
        ("id" := Json.int)
        ("secret"
            := Json.object3 Question
                ("person" := personDecoder)
                ("weapon" := weaponDecoder)
                ("location" := locationDecoder)
        )
        ("bots"
            := Json.list
                (Json.object3
                    Bot
                    ("teamId" := Json.int)
                    ("persons" := Json.list personDecoder)
                    ("weapons" := Json.list weaponDecoder)
                    ("locations" := Json.list locationDecoder)
                )
        )
