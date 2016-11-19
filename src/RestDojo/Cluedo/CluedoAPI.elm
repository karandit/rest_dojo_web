module RestDojo.Cluedo.CluedoAPI exposing (getCluedoGame)

import Http exposing (Request)
import RestDojo.Cluedo.CluedoTypes exposing (..)


-- API end-points ------------------------------------------------------------------------------------------------------


getCluedoGame : String -> Request CluedoGame
getCluedoGame url =
    Http.get url cluedoGameDecoder
