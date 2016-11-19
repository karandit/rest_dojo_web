module RestDojo.Util exposing (..)


avatar : String -> String
avatar name =
    "https://robohash.herokuapp.com/" ++ name ++ ".png"
