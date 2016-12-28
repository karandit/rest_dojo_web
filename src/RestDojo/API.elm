module RestDojo.API
    exposing
        ( getTeams
        , getEvents
        , getBillboard
        , getDojos
        , getPoints
        , getGame
        , postNewTeam
        , postJoinTeam
        , patchAccepTeamMember
        , deleteDenyTeamMember
        )

import Dict exposing (Dict)
import Json.Decode as Json exposing (..)
import Json.Encode as JsonEnc exposing (..)
import Http exposing (Request, Body)
import RestDojo.Types exposing (..)
import RestDojo.Cluedo.CluedoTypes exposing (..)
import RestDojo.Minesweeper.MinesweeperTypes exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getBillboard : List HeaderFlag -> String -> Request Billboard
getBillboard headers url =
    get headers url billboardDecoder


getDojos : List HeaderFlag -> String -> Request (List Dojo)
getDojos headers url =
    get headers url dojosDecoder


getTeams : List HeaderFlag -> String -> Request (List Team)
getTeams headers url =
    get headers url teamsDecoder


postNewTeam : List HeaderFlag -> String -> String -> User -> Request Team
postNewTeam headers url teamName user =
    let
        teamJson =
            JsonEnc.object
                [ ( "teamName", JsonEnc.string teamName )
                , ( "captainName", JsonEnc.string user.name )
                , ( "captainFullname", JsonEnc.string user.fullname )
                , ( "captainPicture", JsonEnc.string user.picture )
                ]
    in
        post headers url (Http.jsonBody teamJson) teamDecoder



-- TODO here the Team is redundant as we don't need the team.Id if it is encoded in the url


postJoinTeam : List HeaderFlag -> String -> Team -> User -> Request TeamMember
postJoinTeam headers url team user =
    let
        body =
            JsonEnc.object
                [ ( "teamId", JsonEnc.string team.id )
                  --TODO: after changing id from int to string we can't use teamId, weneed real url
                , ( "status", JsonEnc.string "entrant" )
                , ( "name", JsonEnc.string user.name )
                , ( "fullname", JsonEnc.string user.fullname )
                , ( "picture", JsonEnc.string user.picture )
                ]
    in
        Http.post url (Http.jsonBody body) teamMemberDecoder


patchAccepTeamMember : List HeaderFlag -> String -> Request TeamMember
patchAccepTeamMember headers url =
    let
        body =
            JsonEnc.object [ ( "status", JsonEnc.string "crew" ) ]
    in
        patch headers url (Http.jsonBody body) teamMemberDecoder


deleteDenyTeamMember : List HeaderFlag -> String -> Request ()
deleteDenyTeamMember headers url =
    delete headers url Http.emptyBody


getPoints : List HeaderFlag -> String -> Request (List GamePoint)
getPoints headers url =
    get headers url gamePointsDecoder


getEvents : List HeaderFlag -> String -> List Team -> Request (List Event)
getEvents headers url teams =
    let
        teamsByTeamId =
            Dict.fromList <| List.map (\team -> ( team.id, team )) teams
    in
        get headers url (eventsDecoder teamsByTeamId)


getGame : List HeaderFlag -> String -> Dojo -> Request Game
getGame headers url dojo =
    get headers url <| gameDecoder dojo



-- HTTP helpers --------------------------------------------------------------------------------------------------------


get : List HeaderFlag -> String -> Decoder a -> Request a
get headers url decoder =
    Http.request
        { method = "GET"
        , headers = List.map (\h -> Http.header h.key h.value) headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


post : List HeaderFlag -> String -> Body -> Decoder a -> Request a
post headers url body decoder =
    Http.request
        { method = "POST"
        , headers = List.map (\h -> Http.header h.key h.value) headers
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


patch : List HeaderFlag -> String -> Body -> Decoder a -> Request a
patch headers url body decoder =
    Http.request
        { method = "PATCH"
        , headers = List.map (\h -> Http.header h.key h.value) headers
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


delete : List HeaderFlag -> String -> Body -> Request ()
delete headers url body =
    Http.request
        { method = "DELETE"
        , headers = List.map (\h -> Http.header h.key h.value) headers
        , url = url
        , body = body
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


billboardDecoder : Decoder Billboard
billboardDecoder =
    Json.map Billboard
        (Json.field "dojosUrl" Json.string)


dojosDecoder : Decoder (List Dojo)
dojosDecoder =
    Json.field "data" <|
        Json.list <|
            Json.map7 (Dojo [] [] Nothing)
                (Json.field "objectId" Json.string)
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


gamePointsDecoder : Decoder (List GamePoint)
gamePointsDecoder =
    Json.field "data" <|
        Json.list <|
            Json.map2 GamePoint
                (Json.field "labelX" Json.string)
                (Json.field "teamPoints" <|
                    Json.list
                        (Json.map2 TeamPoint
                            (Json.field "teamName" Json.string)
                            (Json.field "point" Json.int)
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
    Json.map5 TeamMember
        (Json.field "name" Json.string)
        (Json.field "fullname" Json.string)
        (Json.field "picture" Json.string)
        (Json.field "status" teamMemberStatusDecoder)
        (Json.map
            (Maybe.withDefault "")
            (Json.maybe
                (Json.field "selfUrl" Json.string)
            )
        )


teamDecoder : Decoder Team
teamDecoder =
    Json.map7 Team
        (Json.field "objectId" Json.string)
        (Json.field "name" Json.string)
        (Json.field "descr" Json.string)
        (Json.field "points" Json.int)
        (Json.field "captain" teamMemberDecoder)
        (Json.map
            (Maybe.withDefault [])
            (Json.maybe
                (Json.field "members" <| Json.list teamMemberDecoder)
            )
        )
        (Json.field "joinUrl" Json.string)


teamsDecoder : Decoder (List Team)
teamsDecoder =
    Json.field "data" <|
        Json.list teamDecoder


eventsDecoder : Dict TeamId Team -> Decoder (List Event)
eventsDecoder teamsByTeamId =
    Json.field "data" <|
        Json.list <|
            Json.map (\( gameUrl, teamId ) -> GameWonBy gameUrl (Dict.get teamId teamsByTeamId)) <|
                Json.map2 (,)
                    (Json.field "gameUrl" Json.string)
                    (Json.field "gameWonBy" Json.string)


gameDecoder : Dojo -> Decoder Game
gameDecoder dojo =
    case dojo.dojoType of
        CluedoDojo ->
            Json.map Cluedo cluedoGameDecoder

        MinesweeperDojo ->
            Json.map Minesweeper minesweeperGameDecoder
