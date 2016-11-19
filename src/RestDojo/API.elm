module RestDojo.API exposing (getTeams, getEvents, getBillboard, getDojos, getGame, getPointHistory)

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Http exposing (Request)
import RestDojo.Types exposing (..)
import RestDojo.Cluedo.CluedoTypes exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getBillboard : String -> Request Billboard
getBillboard url =
    Http.get url billboardDecoder


getDojos : String -> Request (List Dojo)
getDojos url =
    Http.get url dojosDecoder


getTeams : String -> Request (List Team)
getTeams url =
    Http.get url teamsDecoder


getPointHistory : String -> Request PointHistory
getPointHistory url =
    Http.get url pointHistoryDecoder


getEvents : String -> List Team -> Request (List Event)
getEvents url teams =
    let
        teamsByTeamId =
            Dict.fromList <| List.map (\team -> ( team.id, team )) teams
    in
        Http.get url (eventsDecoder teamsByTeamId)


getGame : GameUrl -> Request Game
getGame url =
    Http.get url gameDecoder



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


billboardDecoder : Decoder Billboard
billboardDecoder =
    Json.map Billboard
        (Json.field "dojos" Json.string)


dojosDecoder : Decoder (List Dojo)
dojosDecoder =
    Json.list <|
        Json.map7 (Dojo [] [] Nothing)
            (Json.field "id" Json.int)
            (Json.field "label" Json.string)
            (Json.field "state" dojoStateDecoder)
            (Json.field "dojoType" dojoTypeDecoder)
            (Json.field "teamsUrl" Json.string)
            (Json.field "eventsUrl" Json.string)
            (Json.field "pointHistoryUrl" Json.string)


dojoStateDecoder : Json.Decoder DojoState
dojoStateDecoder =
    let
        decodeToType string =
            case string of
                "running" ->
                    Json.succeed Running

                "past" ->
                    Json.succeed Past

                "upcoming" ->
                    Json.succeed Upcoming

                _ ->
                    Json.fail <| "Not valid pattern for decoder to DojoState. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


dojoTypeDecoder : Json.Decoder DojoType
dojoTypeDecoder =
    let
        decodeToType string =
            case string of
                "cluedo" ->
                    Json.succeed Cluedo

                "minesweeper" ->
                    Json.succeed Minesweeper

                _ ->
                    Json.fail <| "Not valid pattern for decoder to DojoType. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


pointHistoryDecoder : Decoder PointHistory
pointHistoryDecoder =
    Json.map2 PointHistory
        (Json.field "games" <| Json.list Json.string)
        (Json.field "teams" <|
            Json.list
                (Json.map2 TeamPoints
                    (Json.field "teamName" Json.string)
                    (Json.field "data" <| Json.list Json.int)
                )
        )


teamsDecoder : Decoder (List Team)
teamsDecoder =
    Json.list <|
        Json.map4 Team
            (Json.field "id" Json.int)
            (Json.field "name" Json.string)
            (Json.field "descr" Json.string)
            (Json.field "points" Json.int)


eventsDecoder : Dict TeamId Team -> Decoder (List Event)
eventsDecoder teamsByTeamId =
    Json.list <|
        Json.map (\( gameUrl, teamId ) -> GameWonBy gameUrl (Dict.get teamId teamsByTeamId)) <|
            Json.map2 (,)
                (Json.field "gameUrl" Json.string)
                (Json.field "gameWonBy" Json.int)


questionDecoder : Decoder Question
questionDecoder =
    Json.map3 Question
        (Json.field "person" personDecoder)
        (Json.field "weapon" weaponDecoder)
        (Json.field "location" locationDecoder)


askedDecoder : Decoder Asked
askedDecoder =
    Json.map2 Asked
        (Json.field "by" Json.string)
        (Json.field "question" questionDecoder)


gameDecoder : Decoder Game
gameDecoder =
    Json.map4 Game
        (Json.field "id" Json.int)
        (Json.field "secret" questionDecoder)
        (Json.field "bots" <|
            Json.list
                (Json.map4
                    Bot
                    (Json.field "teamName" Json.string)
                    (Json.field "persons" <| Json.list personDecoder)
                    (Json.field "weapons" <| Json.list weaponDecoder)
                    (Json.field "locations" <| Json.list locationDecoder)
                )
        )
        (Json.field "rounds" <|
            Json.list
                (Json.oneOf
                    [ (Json.map Interrogate
                        (Json.map2 Interrogation
                            (Json.field "asked" askedDecoder)
                            (Json.field "answered" <|
                                Json.list
                                    (Json.map2 Answered
                                        (Json.field "by" Json.string)
                                        (Json.field "answer" <| Json.oneOf [ Json.null Nothing, Json.map Just Json.string ])
                                    )
                            )
                        )
                      )
                    , (Json.map Accuse
                        (Json.map2 Accusation
                            (Json.field "accused" askedDecoder)
                            (Json.field "answer" Json.bool)
                        )
                      )
                    ]
                )
        )
