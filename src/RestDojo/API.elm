module RestDojo.API
    exposing
        ( getTeams
        , getEvents
        , getBillboard
        , getDojos
        , getPoints
        , getGame
        , postNewTeam
        , putJoinTeam
        , putAccepTeamMember
        , deleteDenyTeamMember
        )

import Date exposing (Date)
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


postNewTeam : List HeaderFlag -> String -> DojoId -> String -> User -> Request Team
postNewTeam headers url dojoId teamName user =
    let
        teamJson =
            JsonEnc.object
                [ ( "dojoId", JsonEnc.string dojoId )
                , ( "teamName", JsonEnc.string teamName )
                , ( "captainName", JsonEnc.string user.name )
                , ( "captainFullname", JsonEnc.string user.fullname )
                , ( "captainPicture", JsonEnc.string user.picture )
                ]
    in
        post headers url (Http.jsonBody teamJson) teamDecoder



-- TODO here the Team is redundant as we don't need the team.Id if it is encoded in the url


putJoinTeam : List HeaderFlag -> String -> Team -> User -> Request TeamMember
putJoinTeam headers url teamToAdd user =
    let
        body =
            JsonEnc.object
                [ ( "members"
                  , JsonEnc.list
                        [ JsonEnc.object
                            [ -- ( "teamId", JsonEnc.string teamToAdd.id )
                              --TODO: after changing id from int to string we can't use teamId, weneed real url
                              ( "___class", JsonEnc.string "teammember" )
                            , ( "status", JsonEnc.string "entrant" )
                            , ( "name", JsonEnc.string user.name )
                            , ( "fullname", JsonEnc.string user.fullname )
                            , ( "picture", JsonEnc.string user.picture )
                            ]
                        ]
                  )
                ]

        teamMembersDecoder =
            Json.field "members" <|
                Json.list teamMemberDecoder

        unwrapTeamMember teamMembers =
            case List.head teamMembers of
                Just firstMember ->
                    Json.succeed firstMember

                Nothing ->
                    Json.fail "Exactly one team member is expected"

        onlyOneTeamMemberDecoder =
            teamMembersDecoder |> Json.andThen unwrapTeamMember
    in
        put headers url (Http.jsonBody body) onlyOneTeamMemberDecoder


putAccepTeamMember : List HeaderFlag -> String -> Request TeamMember
putAccepTeamMember headers url =
    let
        body =
            JsonEnc.object [ ( "status", JsonEnc.string "crew" ) ]
    in
        put headers url (Http.jsonBody body) teamMemberDecoder


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


put : List HeaderFlag -> String -> Body -> Decoder a -> Request a
put headers url body decoder =
    Http.request
        { method = "PUT"
        , headers = List.map (\h -> Http.header h.key h.value) headers
        , url = url
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }



-- patch : List HeaderFlag -> String -> Body -> Decoder a -> Request a
-- patch headers url body decoder =
--     Http.request
--         { method = "PATCH"
--         , headers = List.map (\h -> Http.header h.key h.value) headers
--         , url = url
--         , body = body
--         , expect = Http.expectJson decoder
--         , timeout = Nothing
--         , withCredentials = False
--         }


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
            Json.map8 (Dojo [] [] Nothing)
                (Json.field "objectId" Json.string)
                (Json.field "label" Json.string)
                (Json.field "state" dojoStateDecoder)
                (Json.field "dojoType" dojoTypeDecoder)
                (Json.field "teamsUrl" Json.string)
                (Json.field "addTeamUrl" Json.string)
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
    Json.andThen (\points -> Json.succeed <| List.sortBy (\team -> Date.toTime team.createdAt) points) <|
        Json.field "data" <|
            Json.list gamePointDecoder


gamePointDecoder : Decoder GamePoint
gamePointDecoder =
    Json.map3 GamePoint
        (Json.field "labelX" Json.string)
        (Json.field "created" (Json.float |> Json.andThen floatToDateDecoder))
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


floatToDateDecoder : Float -> Json.Decoder Date
floatToDateDecoder t =
    Json.succeed <| Date.fromTime t


stringToDateDecoder : String -> Json.Decoder Date
stringToDateDecoder s =
    case Date.fromString s of
        Ok date ->
            Json.succeed date

        Err error ->
            Json.fail error


teamDecoder : Decoder Team
teamDecoder =
    Json.map8 (Team 0)
        (Json.field "objectId" Json.string)
        (Json.field "name" Json.string)
        (Json.field "descr" Json.string)
        (Json.field "points" Json.int)
        (Json.field "created" (Json.string |> Json.andThen stringToDateDecoder))
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
    let
        orderedTeams teams =
            teams
                |> List.sortBy (\team -> team.createdAt |> Date.toTime)
                |> List.indexedMap (\idx team -> { team | index = idx })
    in
        Json.andThen (\teams -> Json.succeed (orderedTeams teams)) <|
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
