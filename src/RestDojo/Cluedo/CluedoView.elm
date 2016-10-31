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
        [ viewSecret game.secret
        , viewBots teamsByTeamId game.bots
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


viewBots : Dict TeamId Team -> List Bot -> Html Msg
viewBots teamsByTeamId bots =
    div [] (List.map (viewBot teamsByTeamId) bots)


viewBot : Dict TeamId Team -> Bot -> Html Msg
viewBot teamsByTeamId bot =
    let
        allBotCards =
            (List.map toString bot.persons) ++ (List.map toString bot.locations) ++ (List.map toString bot.weapons)

        cardImgs =
            List.map viewCardSmall allBotCards
    in
        div [] <| (teamImg teamsByTeamId bot.teamId) :: cardImgs


teamImg teamsByTeamId teamId =
    let
        foundTeam =
            Dict.get teamId teamsByTeamId

        avatarAttr =
            case foundTeam of
                Just team ->
                    [ src <| avatar team.name
                    , class "rd-team-avatar"
                    , title team.name
                    ]

                Nothing ->
                    [ class "rd-team-avatar" ]
    in
        img avatarAttr []


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
        roundLabel =
            text <| (++) "#" <| toString <| idx + 1

        askedBy =
            teamImgByName round.asked.by

        askedQuestion =
            viewQuestion round.asked.question

        answeredBy answered =
            [ teamImgByName answered.by ] ++ [ viewCardSmall <| Maybe.withDefault "None" answered.answer ]

        answers =
            List.concatMap answeredBy round.answered
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
