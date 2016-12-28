module RestDojo.Util exposing (..)

import List.Extra


avatar : String -> String
avatar name =
    "https://robohash.herokuapp.com/" ++ name ++ ".png"


fillList : Int -> a -> List a
fillList len defValue =
    List.map (\_ -> defValue) <|
        if len == 0 then
            []
        else
            List.Extra.iterate
                (\x ->
                    if x + 1 < len then
                        Just (x + 1)
                    else
                        Nothing
                )
                0
