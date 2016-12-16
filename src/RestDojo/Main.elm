port module RestDojo.Main exposing (..)

import Html
import Http
import Task
import Navigation exposing (Location)
import RestDojo.Types exposing (..)
import RestDojo.API as API exposing (..)
import RestDojo.View exposing (..)
import RestDojo.Chartjs exposing (..)
import List.Extra
import UrlParser as Url exposing ((</>), s, int, top)


-- MAIN ----------------------------------------------------------------------------------------------------------------


type alias Flags =
    { baseUrl : String
    }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> authentications LoggedIn
        }


parser : Url.Parser (Route -> a) a
parser =
    Url.oneOf
        [ Url.map HomeRoute top
        , Url.map DojoRoute (s "dojos" </> int)
        ]


getRoute : Location -> Route
getRoute location =
    let
        maybeRoute =
            Url.parseHash parser location
    in
        case maybeRoute of
            Just route ->
                route

            Nothing ->
                NotFoundRoute


port chart : ChartInput -> Cmd msg


port auth0 : String -> Cmd msg


port authentications : (User -> msg) -> Sub msg


mapToChartInput : PointHistory -> ChartInput
mapToChartInput pointHistory =
    { labels = pointHistory.games
    , datasets =
        pointHistory.teams
            |> List.Extra.zip [ "#7e5ae2", "#e25abc", "#e25a77", "#7e9ce2", "#f78764", "#1784c7" ]
            |> List.map (\( color, teamPoints ) -> { label = teamPoints.teamName, data = teamPoints.data, borderColor = color })
    }



-- MODEL ---------------------------------------------------------------------------------------------------------------


initModel : Flags -> Location -> ( Model, Cmd Msg )
initModel flags location =
    { billboard = Billboard ""
    , route = HomeRoute
    , dojos = []
    , user = Nothing
    }
        ! [ loadBillboard flags.baseUrl ]


loadBillboard : String -> Cmd Msg
loadBillboard url =
    Http.send LoadBillboard (API.getBillboard url)


loadDojos : String -> Cmd Msg
loadDojos url =
    Http.send LoadDojos (API.getDojos url)


loadTeams : Dojo -> Cmd Msg
loadTeams dojo =
    Http.send (LoadTeams dojo) (API.getTeams dojo.teamsUrl)


loadPointHistory : Dojo -> Cmd Msg
loadPointHistory dojo =
    Http.send LoadPointHistory (API.getPointHistory dojo.pointHistoryUrl)


loadEvents : Dojo -> List Team -> Cmd Msg
loadEvents dojo teams =
    Http.send (LoadEvents dojo) (API.getEvents dojo.eventsUrl teams)


loadGame : Dojo -> GameUrl -> Cmd Msg
loadGame dojo gameUrl =
    Http.send (LoadGame dojo) (API.getGame gameUrl dojo)


createTeam : Dojo -> String -> Maybe User -> List (Cmd Msg)
createTeam dojo teamName loggedUser =
    case loggedUser of
        Just user ->
            [ Http.send (CreatedTeam dojo) (API.postNewTeam dojo.teamsUrl teamName user) ]

        Nothing ->
            []


joinTeam : Dojo -> Team -> Maybe User -> List (Cmd Msg)
joinTeam dojo team loggedUser =
    case loggedUser of
        Just user ->
            [ Http.send (JoinedTeamAsEntrant dojo team) (API.postJoinTeam team.joinUrl team user) ]

        Nothing ->
            []


accepTeamMember : Dojo -> Team -> TeamMember -> List (Cmd Msg)
accepTeamMember dojo team teamMember =
    [ Http.send (JoinedTeamAsCrew dojo team) (API.patchAccepTeamMember teamMember.selfUrl) ]



-- UPDATE --------------------------------------------------------------------------------------------------------------


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "__msg" msg of
        LoadBillboard (Ok billboard) ->
            { model | billboard = billboard } ! [ loadDojos billboard.dojosUrl ]

        LoadDojos (Ok loadedDojos) ->
            { model | dojos = loadedDojos } ! []

        LoadPointHistory (Ok pointHistory) ->
            model ! [ chart <| mapToChartInput pointHistory ]

        LoadGame dojo (Ok game) ->
            { model | route = GameRoute dojo.id game } ! []

        LoadTeams oldDojo (Ok loadedTeams) ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = loadedTeams }) model.dojos } ! [ loadEvents oldDojo loadedTeams ]

        LoadEvents oldDojo (Ok loadedEvents) ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | events = loadedEvents }) model.dojos } ! []

        CreateTeam dojo teamName ->
            model ! (createTeam dojo teamName model.user)

        CreatedTeam oldDojo (Ok newTeam) ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = dojo.teams ++ [ newTeam ], dialog = Nothing }) model.dojos } ! []

        JoinTeam dojo team ->
            model ! (joinTeam dojo team model.user)

        JoinedTeamAsEntrant oldDojo oldTeam (Ok newTeamMember) ->
            let
                addTeamMember dojo =
                    updateTeam oldTeam.id (\team -> { team | members = team.members ++ [ newTeamMember ] }) dojo.teams
            in
                { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = addTeamMember dojo, dialog = Nothing }) model.dojos } ! []

        AcceptJoinTeam dojo team teamMember ->
            model ! (accepTeamMember dojo team teamMember)

        JoinedTeamAsCrew oldDojo oldTeam (Ok newTeamMember) ->
            let
                updateTeamMember_ team =
                    updateTeamMember newTeamMember.name (\_ -> newTeamMember) team.members

                updateTeam_ dojo =
                    updateTeam oldTeam.id (\team -> { team | members = updateTeamMember_ team }) dojo.teams
            in
                { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | teams = updateTeam_ dojo }) model.dojos } ! []

        SelectHome ->
            { model | route = HomeRoute } ! []

        SelectDojo dojo ->
            { model | route = DojoRoute dojo.id } ! [ loadTeams dojo, loadPointHistory dojo ]

        SelectGame dojo gameUrl ->
            model ! [ loadGame dojo gameUrl ]

        LoginPushed ->
            model ! [ auth0 "test" ]

        LoggedIn loggeduser ->
            { model | user = Just loggeduser } ! []

        ShowEditTeamDialog oldDojo team ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Just (EditTeamDialog team.id) }) model.dojos } ! []

        ShowJoinTeamDialog oldDojo team ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Just (JoinTeamDialog team.id) }) model.dojos } ! []

        ShowCreateTeamDialog oldDojo ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Just (CreateTeamDialog "") }) model.dojos } ! []

        EditTeamNameInDialog oldDojo teamName ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Just (CreateTeamDialog teamName) }) model.dojos } ! []

        CloseTeamDialog oldDojo ->
            { model | dojos = updateDojo oldDojo.id (\dojo -> { dojo | dialog = Nothing }) model.dojos } ! []

        UrlChange location ->
            model ! []

        _ ->
            let
                _ =
                    Debug.log "__error" msg
            in
                model ! []


updateDojo : Int -> (Dojo -> Dojo) -> List Dojo -> List Dojo
updateDojo dojoId updater dojos =
    updateXXX (\dojo -> dojo.id == dojoId) updater dojos


updateTeam : Int -> (Team -> Team) -> List Team -> List Team
updateTeam teamId updater teams =
    updateXXX (\team -> team.id == teamId) updater teams


updateTeamMember : String -> (TeamMember -> TeamMember) -> List TeamMember -> List TeamMember
updateTeamMember teamMemberName updater teamMembers =
    updateXXX (\teamMember -> teamMember.name == teamMemberName) updater teamMembers


updateXXX : (a -> Bool) -> (a -> a) -> List a -> List a
updateXXX pred updater inp =
    List.map
        (\x ->
            if pred x then
                updater x
            else
                x
        )
        inp
