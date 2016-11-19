module RestDojo.Minesweeper.MinesweeperTypes exposing (..)

import Json.Decode as Json exposing (..)


-- Types ---------------------------------------------------------------------------------------------------------------


type alias MinesweeperGame =
    { id : Int
    , board : String
    }



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


minesweeperGameDecoder : Decoder MinesweeperGame
minesweeperGameDecoder =
    Json.map2 MinesweeperGame
        (Json.field "id" Json.int)
        (Json.field "board" Json.string)
