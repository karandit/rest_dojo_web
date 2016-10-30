module RestDojo.Types exposing (..)

import Http


type Route
    = HomeRoute
    | DojoRoute Dojo


type alias DojoId =
    Int


type alias Dojo =
    { id : TeamId
    , label : String
    , state : String
    }


type alias TeamId =
    Int


type alias Team =
    { id : TeamId
    , name : String
    , descr : String
    , points : Int
    }


type Event
    = GameWonBy (Maybe Team)


type alias Billboard =
    { dojosUrl : String
    , teamsUrl : String
    , eventsUrl : String
    }


type alias Model =
    { billboard : Billboard
    , route : Route
    , dojos : List Dojo
    , teams : List Team
    , events : List Event
    }


type Msg
    = BillboardLoadSucceed Billboard
    | DojosLoadSucceed (List Dojo)
    | SelectDojo Dojo
    | TeamsLoadSucceed (List Team)
    | EventsLoadSucceed (List Event)
    | ErrorOccured Http.Error
