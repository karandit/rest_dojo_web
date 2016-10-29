module RestDojo.Types exposing (..)


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
