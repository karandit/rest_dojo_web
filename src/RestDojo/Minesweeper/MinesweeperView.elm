module RestDojo.Minesweeper.MinesweeperView exposing (view)

import Html exposing (Html, text)
import RestDojo.Minesweeper.MinesweeperTypes exposing (..)
import RestDojo.Types exposing (..)


view : Dojo -> MinesweeperGame -> List (Html Msg)
view dojo game =
    [ text game.board
    ]
