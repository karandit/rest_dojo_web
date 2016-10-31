module RestDojo.Cluedo.CluedoView exposing (view)

import Html exposing (Html, text, div, span, article, hr, img)
import Html.Attributes exposing (class, title, src, width, height)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


view : Dojo -> Game -> List (Html Msg)
view dojo game =
    [ viewSecret game.secret
    , viewBots game.bots
    , hr [] []
    , viewRounds game.rounds
    ]


viewSecret : Question -> Html Msg
viewSecret question =
    div [] <| viewQuestionWithSize viewCard question


viewQuestion : Question -> List (Html Msg)
viewQuestion =
    viewQuestionWithSize viewCardSmall


viewQuestionWithSize : (String -> Html Msg) -> Question -> List (Html Msg)
viewQuestionWithSize displayer question =
    List.map displayer [ toString question.person, toString question.weapon, toString question.location ]


viewBots : List Bot -> Html Msg
viewBots bots =
    div [] <| List.map viewBot bots


viewBot : Bot -> Html Msg
viewBot bot =
    let
        allBotCards =
            (List.map toString bot.persons) ++ (List.map toString bot.locations) ++ (List.map toString bot.weapons)

        cardImgs =
            List.map viewCardSmall allBotCards
    in
        div [] <| (teamImgByName bot.teamName) :: cardImgs


teamImgByName teamName =
    let
        avatarAttr =
            [ src <| avatar teamName
            , class "rd-team-avatar"
            , title teamName
            ]
    in
        img avatarAttr []


viewRounds : List Round -> Html Msg
viewRounds rounds =
    div [] <| List.map viewRound <| List.indexedMap (,) rounds


viewRound : ( Int, Round ) -> Html Msg
viewRound ( idx, round ) =
    let
        roundType =
            case round of
                Interrogate _ ->
                    "Asked #"

                Accuse _ ->
                    "Accused #"

        roundLabel =
            text <| (++) roundType <| toString <| idx + 1

        asked =
            case round of
                Interrogate interrogation ->
                    interrogation.asked

                Accuse accusation ->
                    accusation.asked

        askedBy =
            teamImgByName asked.by

        askedQuestion =
            viewQuestion asked.question

        answeredBy answered =
            [ teamImgByName answered.by ] ++ [ viewCardSmall <| Maybe.withDefault "None" answered.answer ]

        answers =
            case round of
                Interrogate interrogation ->
                    List.concatMap answeredBy interrogation.answered

                Accuse accusation ->
                    [ text <| toString accusation.answer ]
    in
        div [] <| [ roundLabel, askedBy ] ++ askedQuestion ++ answers


viewCard : String -> Html Msg
viewCard =
    viewCardWithSize 144 180


viewCardSmall : String -> Html Msg
viewCardSmall =
    viewCardWithSize 80 100


viewCardWithSize : Int -> Int -> String -> Html Msg
viewCardWithSize w h cardName =
    img [ src <| "img/cards/" ++ cardName ++ ".png", width w, height h, title cardName ] []
