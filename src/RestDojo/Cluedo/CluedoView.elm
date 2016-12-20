module RestDojo.Cluedo.CluedoView exposing (view)

import Html exposing (Html, text, div, span, article, h2, hr, img, strong)
import Html.Attributes exposing (class, title, src, width, height)
import RestDojo.Cluedo.CluedoTypes exposing (..)
import RestDojo.Types exposing (..)
import RestDojo.Util exposing (..)


view : Dojo -> CluedoGame -> List (Html Msg)
view dojo game =
    [ div [ class "rd-cluedo" ]
        [ div [ class "rd-cluedo-panel" ]
            [ h2 [] [ text "Secret" ]
            , viewSecret game.secret
            , h2 [] [ text "Cards in hands" ]
            , viewBots game.bots
            ]
        , div [ class "rd-cluedo-panel rd-cluedo-rounds" ]
            [ h2 [] [ text "Rounds" ]
            , viewRounds game.rounds
            ]
        ]
    ]


viewSecret : Question -> Html Msg
viewSecret question =
    div [ class "rd-cluedo-cards" ] <| viewQuestionWithSize viewCard question


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
        div [ class "rd-cluedo-cards" ] <| (teamImgByName bot.teamName Nothing) :: cardImgs


type SecondLabel
    = Asked
    | Accused
    | Answered


teamImgByName : String -> Maybe SecondLabel -> Html Msg
teamImgByName teamName secondLabel =
    let
        avatarAttr =
            [ src <| avatar teamName, class "rd-team-avatar", title teamName ]

        secondStrong =
            secondLabel
                |> Maybe.map (\lbl -> [ strong [ class "rd-cluedo-item-label" ] [ text <| String.toLower <| toString lbl ] ])
                |> Maybe.withDefault []
    in
        span [ class "rd-cluedo-item" ] <|
            [ img avatarAttr []
            , strong [ class "rd-cluedo-item-label" ] [ text teamName ]
            ]
                ++ secondStrong


viewRounds : List Round -> Html Msg
viewRounds rounds =
    div [] <| List.map viewRound <| List.indexedMap (,) rounds


viewRound : ( Int, Round ) -> Html Msg
viewRound ( idx, round ) =
    let
        roundLabel =
            text <| (++) "#" <| toString <| idx + 1

        asked =
            case round of
                Interrogate interrogation ->
                    interrogation.asked

                Accuse accusation ->
                    accusation.asked

        secLabel =
            case round of
                Interrogate _ ->
                    Asked

                Accuse _ ->
                    Accused

        askedBy =
            teamImgByName asked.by <| Just secLabel

        askedQuestion =
            viewQuestion asked.question

        answeredBy answered =
            [ teamImgByName answered.by <| Just Answered ] ++ [ viewCardSmall <| Maybe.withDefault "None" answered.answer ]

        answers =
            case round of
                Interrogate interrogation ->
                    List.concatMap answeredBy interrogation.answered

                Accuse accusation ->
                    [ strong [ class "rd-cluedo-item-label" ] [ text <| toString accusation.answer ] ]
    in
        div [ class "rd-cluedo-round rd-cluedo-cards" ] <| [ roundLabel, askedBy ] ++ askedQuestion ++ answers


viewCard : String -> Html Msg
viewCard =
    viewCardWithSize 144 180


viewCardSmall : String -> Html Msg
viewCardSmall =
    viewCardWithSize 80 100


viewCardWithSize : Int -> Int -> String -> Html Msg
viewCardWithSize w h cardName =
    span [ class "rd-cluedo-item" ]
        [ img [ src <| "img/cards/" ++ cardName ++ ".png", width w, height h, title cardName ] []
        , strong [ class "rd-cluedo-item-label" ] [ text cardName ]
        ]
