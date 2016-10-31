module RestDojo.Cluedo.CluedoView exposing (view)

import Dict exposing (Dict)
import Html exposing (Html, text, div, span, article, hr, img)
import Html.Attributes exposing (class, title, src, width, height)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


view : Dojo -> Game -> List (Html Msg)
view dojo game =
    let
        teamsByTeamId =
            Dict.fromList <| List.map (\team -> ( team.id, team )) dojo.teams
    in
        [ div [] <| List.map viewCard [ toString game.secret.person, toString game.secret.weapon, toString game.secret.location ]
        , hr [] []
        ]
            ++ (List.map (viewBot teamsByTeamId) game.bots)


allBotCards : Bot -> List String
allBotCards bot =
    (List.map toString bot.persons) ++ (List.map toString bot.locations) ++ (List.map toString bot.weapons)


viewBot : Dict TeamId Team -> Bot -> Html Msg
viewBot teamsByTeamId bot =
    let
        foundTeam =
            Dict.get bot.teamId teamsByTeamId

        avatarAttr =
            case foundTeam of
                Just team ->
                    [ src <| avatar team.name
                    , class <| "rd-team-avatar rd-team-" ++ toString team.id
                    , title team.name
                    ]

                Nothing ->
                    [ class "rd-team-avatar" ]

        botImg =
            img avatarAttr []

        cardImgs =
            List.map viewCardSmall <| allBotCards bot
    in
        span [] <| botImg :: cardImgs


viewCard : String -> Html Msg
viewCard =
    viewCardWithSize 144 180


viewCardSmall : String -> Html Msg
viewCardSmall =
    viewCardWithSize 80 100


viewCardWithSize : Int -> Int -> String -> Html Msg
viewCardWithSize w h cardName =
    img [ src <| "img/cards/" ++ cardName ++ ".png", width w, height h, title cardName ] []
