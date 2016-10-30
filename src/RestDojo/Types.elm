module RestDojo.Types exposing (..)

import Http


type Route
    = HomeRoute
    | DojoRoute DojoId


type alias DojoId =
    Int


type alias Dojo =
    { teams : List Team
    , events : List Event
    , id : TeamId
    , label : String
    , state : String
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


type Event
    = GameWonBy (Maybe Team)


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
    | TeamsLoadSucceed Dojo (List Team)
    | EventsLoadSucceed Dojo (List Event)
    | ErrorOccured Http.Error
