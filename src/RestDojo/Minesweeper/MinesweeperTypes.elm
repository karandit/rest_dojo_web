module RestDojo.Minesweeper.MinesweeperTypes exposing (..)

import Json.Decode as Json exposing (..)


-- Types ---------------------------------------------------------------------------------------------------------------


type alias MinesweeperGame =
    { id : String
    , board : String
    }



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


minesweeperGameDecoder : Decoder MinesweeperGame
minesweeperGameDecoder =
    Json.map2 MinesweeperGame
        (Json.field "objectId" Json.string)
        (Json.field "content" (Json.field "board" Json.string))
