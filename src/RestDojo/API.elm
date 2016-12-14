module RestDojo.API
    exposing
        ( getTeams
        , getEvents
        , getBillboard
        , getDojos
        , getPointHistory
        , getGame
        , postNewTeam
        , postJoinTeam
        )

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Json.Encode as JsonEnc exposing (..)
import Http exposing (Request)
import RestDojo.Types exposing (..)
import RestDojo.Cluedo.CluedoTypes exposing (..)
import RestDojo.Minesweeper.MinesweeperTypes exposing (..)


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


postNewTeam : String -> String -> User -> Request Team
postNewTeam url teamName user =
    let
        teamJson =
            JsonEnc.object
                [ ( "teamName", JsonEnc.string teamName )
                , ( "captainName", JsonEnc.string user.nickname )
                ]
    in
        Http.post url (Http.jsonBody teamJson) teamDecoder



-- TODO here the Team is redundant as we don't need the team.Id if it is encoded in the url


postJoinTeam : String -> Team -> User -> Request TeamMember
postJoinTeam url team user =
    let
        json =
            JsonEnc.object
                [ ( "teamId", JsonEnc.int team.id )
                , ( "status", JsonEnc.string "entrant" )
                , ( "name", JsonEnc.string user.nickname )
                ]
    in
        Http.post url (Http.jsonBody json) teamMemberDecoder


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


getGame : String -> Dojo -> Request Game
getGame url dojo =
    Http.get url <| gameDecoder dojo



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
                    Json.succeed CluedoDojo

                "minesweeper" ->
                    Json.succeed MinesweeperDojo

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


teamMemberStatusDecoder : Json.Decoder TeamMemberStatus
teamMemberStatusDecoder =
    let
        decodeToType string =
            case string of
                "crew" ->
                    Json.succeed Crew

                "entrant" ->
                    Json.succeed Entrant

                _ ->
                    Json.fail <| "Not valid pattern for decoder to TeamMemberStatus. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


teamMemberDecoder : Json.Decoder TeamMember
teamMemberDecoder =
    Json.map2 TeamMember
        (Json.field "name" Json.string)
        (Json.field "status" teamMemberStatusDecoder)


teamDecoder : Decoder Team
teamDecoder =
    Json.map7 Team
        (Json.field "id" Json.int)
        (Json.field "name" Json.string)
        (Json.field "descr" Json.string)
        (Json.field "points" Json.int)
        (Json.field "captain" Json.string)
        (Json.map
            (Maybe.withDefault [])
            (Json.maybe
                (Json.field "members" <| Json.list teamMemberDecoder)
            )
        )
        (Json.field "joinUrl" Json.string)


teamsDecoder : Decoder (List Team)
teamsDecoder =
    Json.list teamDecoder


eventsDecoder : Dict TeamId Team -> Decoder (List Event)
eventsDecoder teamsByTeamId =
    Json.list <|
        Json.map (\( gameUrl, teamId ) -> GameWonBy gameUrl (Dict.get teamId teamsByTeamId)) <|
            Json.map2 (,)
                (Json.field "gameUrl" Json.string)
                (Json.field "gameWonBy" Json.int)


gameDecoder : Dojo -> Decoder Game
gameDecoder dojo =
    case dojo.dojoType of
        CluedoDojo ->
            Json.map Cluedo cluedoGameDecoder

        MinesweeperDojo ->
            Json.map Minesweeper minesweeperGameDecoder
