module RestDojo.Main exposing (..)

import Html exposing (Html, text, div, header, h1)
import Html.App


-- MAIN ----------------------------------------------------------------------------------------------------------------


main : Program Never
main =
    Html.App.program
        { init = initModel
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL ---------------------------------------------------------------------------------------------------------------


type alias Model =
    { players : String
    }


initModel : ( Model, Cmd Msg )
initModel =
    { players = ""
    }
        ! []



-- UPDATE --------------------------------------------------------------------------------------------------------------


type Msg
    = NotYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []



-- VIEW ----------------------------------------------------------------------------------------------------------------


view : Model -> Html Msg
view model =
    div []
        [ header []
            [ h1 [] [ text "Rest Dojo" ]
            ]
        ]
