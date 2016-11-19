module RestDojo.Types exposing (..)

import Http
import RestDojo.Cluedo.CluedoTypes exposing (..)
import RestDojo.Minesweeper.MinesweeperTypes exposing (..)


type alias User =
    { name : String
    , picture : String
    , nickname : String
    }


type Route
    = HomeRoute
    | DojoRoute DojoId
    | GameRoute DojoId Game


type alias DojoId =
    Int


type DojoState
    = Past
    | Running
    | Upcoming


type DojoType
    = CluedoDojo
    | MinesweeperDojo


type alias Dojo =
    { teams : List Team
    , events : List Event
    , dialog : Maybe Team
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


type alias Team =
    { id : TeamId
    , name : String
    , descr : String
    , points : Int
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
    }


type Game
    = Cluedo CluedoGame
    | Minesweeper MinesweeperGame


type Msg
    = LoadBillboard (Result Http.Error Billboard)
    | LoadDojos (Result Http.Error (List Dojo))
    | LoadTeams Dojo (Result Http.Error (List Team))
    | LoadEvents Dojo (Result Http.Error (List Event))
    | LoadPointHistory (Result Http.Error PointHistory)
    | LoadGame Dojo (Result Http.Error Game)
    | SelectDojo Dojo
    | SelectGame Dojo GameUrl
    | LoginPushed
    | LoggedIn User
    | ShowTeamDialog Dojo Team
    | CloseTeamDialog Dojo
