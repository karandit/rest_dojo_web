module RestDojo.Types exposing (..)

import Http
import RestDojo.Cluedo.CluedoTypes exposing (..)


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


type alias Dojo =
    { teams : List Team
    , events : List Event
    , dialog : Maybe Team
    , id : DojoId
    , label : String
    , state : DojoState
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


type alias GameId =
    Int


type alias Question =
    { person : Person
    , weapon : Weapon
    , location : Location
    }


type alias Bot =
    { teamName : String
    , persons : List Person
    , weapons : List Weapon
    , locations : List Location
    }


type alias Asked =
    { by : String
    , question : Question
    }


type alias Answered =
    { by : String
    , answer : Maybe String
    }


type Round
    = Interrogate Interrogation
    | Accuse Accusation


type alias Interrogation =
    { asked : Asked
    , answered : List Answered
    }


type alias Accusation =
    { asked : Asked
    , answer : Bool
    }


type alias Game =
    { id : GameId
    , secret : Question
    , bots : List Bot
    , rounds : List Round
    }


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


type Msg
    = BillboardLoadSucceed (Result Http.Error Billboard)
    | DojosLoadSucceed (Result Http.Error (List Dojo))
    | SelectDojo Dojo
    | SelectGame Dojo GameUrl
    | PointHistoryLoadSucceed (Result Http.Error PointHistory)
    | GameLoadSucceed Dojo (Result Http.Error Game)
    | TeamsLoadSucceed Dojo (Result Http.Error (List Team))
    | EventsLoadSucceed Dojo (Result Http.Error (List Event))
    | LoginPushed
    | LoggedIn User
    | ShowTeamDialog Dojo Team
    | CloseTeamDialog Dojo
