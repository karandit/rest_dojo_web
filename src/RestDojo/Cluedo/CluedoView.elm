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
        , viewRounds teamsByTeamId game.rounds
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
        span [] <| (teamImg teamsByTeamId bot.teamId) :: cardImgs


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


viewRounds : Dict TeamId Team -> List Round -> Html Msg
viewRounds teamsByTeamId rounds =
    div [] (List.map (viewRound teamsByTeamId) rounds)


viewRound : Dict TeamId Team -> Round -> Html Msg
viewRound teamsByTeamId round =
    let
        askedBy =
            teamImg teamsByTeamId round.asked.by

        askedQuestion =
            viewQuestion round.asked.question

        answers =
            List.map teamImgByName <| List.map .by round.answered
    in
        div [] <| [ askedBy ] ++ askedQuestion ++ answers


viewCard : String -> Html Msg
viewCard =
    viewCardWithSize 144 180


viewCardSmall : String -> Html Msg
viewCardSmall =
    viewCardWithSize 80 100


viewCardWithSize : Int -> Int -> String -> Html Msg
viewCardWithSize w h cardName =
    img [ src <| "img/cards/" ++ cardName ++ ".png", width w, height h, title cardName ] []
