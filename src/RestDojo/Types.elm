module RestDojo.Types exposing (..)

import Http
import RestDojo.Cluedo.CluedoTypes exposing (..)
import RestDojo.Minesweeper.MinesweeperTypes exposing (..)
import Navigation


type alias User =
    { fullname : String
    , picture : String
    , name : String
    }


type Route
    = HomeRoute
    | DojoRoute DojoId
    | GameRoute DojoId Game
    | NotFoundRoute


type alias AlertId =
    Int


type alias Alert =
    { id : AlertId
    , message : String
    }


type alias DojoId =
    Int


type DojoState
    = Past
    | Running
    | Upcoming


type DojoType
    = CluedoDojo
    | MinesweeperDojo


type DojoDialog
    = EditTeamDialog TeamId
    | CreateTeamDialog String
    | JoinTeamDialog TeamId


type alias Dojo =
    { teams : List Team
    , events : List Event
    , dialog : Maybe DojoDialog
    , id : DojoId
    , label : String
    , state : DojoState
    , dojoType : DojoType
    , teamsUrl : String
    , eventsUrl : String
    , pointHistoryUrl : String
    }


type alias TeamId =
    Int


type TeamMemberStatus
    = Crew
    | Entrant


type alias TeamMember =
    { name : String
    , fullname : String
    , picture : String
    , status : TeamMemberStatus
    , selfUrl : String
    }


type alias Team =
    { id : TeamId
    , name : String
    , descr : String
    , points : Int
    , captain : TeamMember
    , members : List TeamMember
    , joinUrl : String
    }


type alias GameUrl =
    String


type Event
    = GameWonBy GameUrl (Maybe Team)


type alias TeamPoints =
    { teamName : String
    , data : List Int
    }


type alias PointHistory =
    { games : List String
    , teams : List TeamPoints
    }


type alias Billboard =
    { dojosUrl : String
    }


type alias Model =
    { billboard : Billboard
    , route : Route
    , dojos : List Dojo
    , user : Maybe User
    , alerts : List Alert
    , nextAlertId : Int
    }


type Game
    = Cluedo CluedoGame
    | Minesweeper MinesweeperGame


type Msg
    = UrlChange Navigation.Location
    | LoadBillboard (Result Http.Error Billboard)
    | LoadDojos (Result Http.Error (List Dojo))
    | LoadTeams Dojo (Result Http.Error (List Team))
    | LoadEvents Dojo (Result Http.Error (List Event))
    | LoadPointHistory (Result Http.Error PointHistory)
    | LoadGame Dojo (Result Http.Error Game)
    | CreateTeam Dojo String
    | CreatedTeam Dojo (Result Http.Error Team)
    | JoinTeam Dojo Team
    | JoinedTeamAsEntrant Dojo Team (Result Http.Error TeamMember)
    | AcceptJoinTeam Dojo Team TeamMember
    | JoinedTeamAsCrew Dojo Team (Result Http.Error TeamMember)
    | DenyJoinTeam Dojo Team TeamMember
    | LeftTeam Dojo Team TeamMember (Result Http.Error ())
    | SelectHome
    | SelectDojo Dojo
    | SelectGame Dojo GameUrl
    | LoginPushed
    | LoggedIn User
    | ShowEditTeamDialog Dojo Team
    | ShowCreateTeamDialog Dojo
    | ShowJoinTeamDialog Dojo Team
    | EditTeamNameInDialog Dojo String
    | CloseTeamDialog Dojo
    | CloseAlert AlertId
