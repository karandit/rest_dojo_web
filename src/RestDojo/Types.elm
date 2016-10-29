module RestDojo.Types exposing (..)


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
    { teamsUrl : String
    , eventsUrl : String
    }
