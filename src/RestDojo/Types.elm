module RestDojo.Types exposing (..)


type alias TeamId =
    Int


type alias Team =
    { id : TeamId
    , name : String
    , descr : String
    , points : Int
    }


type alias Event =
    { teamId : TeamId
    , teamName : String
    }
