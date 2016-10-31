module RestDojo.Util exposing (..)


avatar : String -> String
avatar name =
    "http://robohash.herokuapp.com/" ++ name ++ ".png"
