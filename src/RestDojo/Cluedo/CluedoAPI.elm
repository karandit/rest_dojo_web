module RestDojo.Cluedo.CluedoAPI exposing (getGame)

import Http exposing (Request)
import RestDojo.Cluedo.CluedoTypes exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getGame : String -> Request Game
getGame url =
    Http.get url gameDecoder
