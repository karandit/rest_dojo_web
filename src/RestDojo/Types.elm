module RestDojo.Types exposing (..)

import Http
import RestDojo.Cluedo.CluedoTypes exposing (..)


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
    , id : DojoId
    , label : String
    , state : DojoState
    , teamsUrl : String
    , eventsUrl : String
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
    { teamId : TeamId
    , persons : List Person
    , weapons : List Weapon
    , locations : List Location
    }


type alias Asked =
    { by : TeamId
    , question : Question
    }


type alias Answered =
    { by : String
    }


type alias Round =
    { asked : Asked
    , answered : List Answered
    }


type alias Game =
    { id : GameId
    , secret : Question
    , bots : List Bot
    , rounds : List Round
    }


type Event
    = GameWonBy GameUrl (Maybe Team)


type alias Billboard =
    { dojosUrl : String
    }


type alias Model =
    { billboard : Billboard
    , route : Route
    , dojos : List Dojo
    }


type Msg
    = BillboardLoadSucceed Billboard
    | DojosLoadSucceed (List Dojo)
    | SelectDojo Dojo
    | SelectGame Dojo GameUrl
    | GameLoadSucceed Dojo Game
    | TeamsLoadSucceed Dojo (List Team)
    | EventsLoadSucceed Dojo (List Event)
    | ErrorOccured Http.Error
