module RestDojo.Cluedo.CluedoTypes exposing (..)

import Json.Decode as Json exposing (..)


-- Types ---------------------------------------------------------------------------------------------------------------


type Location
    = BedRoom
    | Billiards
    | Conservatory
    | Kitchen
    | Library
    | Lounge
    | Stairs
    | Studio
    | TrophyHall


type Person
    = ColMustard
    | MrsWhite
    | MsPeacock
    | MsScarlett
    | ProfPlum
    | RevGreen


type Weapon
    = Candlestick
    | IcePick
    | Poison
    | Poker
    | Revolver
    | Shears


type Card
    = LocationCard Location
    | PersonCard Person
    | WeaponCard Weapon


type alias Question =
    { person : Person
    , weapon : Weapon
    , location : Location
    }


type alias Bot =
    { teamName : String
    , persons : List Person
    , weapons : List Weapon
    , locations : List Location
    }


type alias Asked =
    { by : String
    , question : Question
    }


type alias Answered =
    { by : String
    , answer : Maybe String
    }


type Round
    = Interrogate Interrogation
    | Accuse Accusation


type alias Interrogation =
    { asked : Asked
    , answered : List Answered
    }


type alias Accusation =
    { asked : Asked
    , answer : Bool
    }


type alias GameId =
    Int


type alias CluedoGame =
    { id : GameId
    , secret : Question
    , bots : List Bot
    , rounds : List Round
    }



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


personDecoder : Json.Decoder Person
personDecoder =
    let
        decodeToType string =
            case string of
                "ColMustard" ->
                    Json.succeed ColMustard

                "MrsWhite" ->
                    Json.succeed MrsWhite

                "MsPeacock" ->
                    Json.succeed MsPeacock

                "MsScarlett" ->
                    Json.succeed MsScarlett

                "ProfPlum" ->
                    Json.succeed ProfPlum

                "RevGreen" ->
                    Json.succeed RevGreen

                _ ->
                    Json.fail <| "Not valid pattern for decoder to Person. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


weaponDecoder : Json.Decoder Weapon
weaponDecoder =
    let
        decodeToType string =
            case string of
                "Candlestick" ->
                    Json.succeed Candlestick

                "IcePick" ->
                    Json.succeed IcePick

                "Poison" ->
                    Json.succeed Poison

                "Poker" ->
                    Json.succeed Poker

                "Revolver" ->
                    Json.succeed Revolver

                "Shears" ->
                    Json.succeed Shears

                _ ->
                    Json.fail <| "Not valid pattern for decoder to Weapon. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


locationDecoder : Json.Decoder Location
locationDecoder =
    let
        decodeToType string =
            case string of
                "BedRoom" ->
                    Json.succeed BedRoom

                "Billiards" ->
                    Json.succeed Billiards

                "Conservatory" ->
                    Json.succeed Conservatory

                "Kitchen" ->
                    Json.succeed Kitchen

                "Library" ->
                    Json.succeed Library

                "Lounge" ->
                    Json.succeed Lounge

                "Stairs" ->
                    Json.succeed Stairs

                "Studio" ->
                    Json.succeed Studio

                "TrophyHall" ->
                    Json.succeed TrophyHall

                _ ->
                    Json.fail <| "Not valid pattern for decoder to Weapon. Pattern: " ++ (toString string)
    in
        Json.string |> Json.andThen decodeToType


questionDecoder : Decoder Question
questionDecoder =
    Json.map3 Question
        (Json.field "person" personDecoder)
        (Json.field "weapon" weaponDecoder)
        (Json.field "location" locationDecoder)


askedDecoder : Decoder Asked
askedDecoder =
    Json.map2 Asked
        (Json.field "by" Json.string)
        (Json.field "question" questionDecoder)


cluedoGameDecoder : Decoder CluedoGame
cluedoGameDecoder =
    Json.map4 CluedoGame
        (Json.field "id" Json.int)
        (Json.field "secret" questionDecoder)
        (Json.field "bots" <|
            Json.list
                (Json.map4
                    Bot
                    (Json.field "teamName" Json.string)
                    (Json.field "persons" <| Json.list personDecoder)
                    (Json.field "weapons" <| Json.list weaponDecoder)
                    (Json.field "locations" <| Json.list locationDecoder)
                )
        )
        (Json.field "rounds" <|
            Json.list
                (Json.oneOf
                    [ (Json.map Interrogate
                        (Json.map2 Interrogation
                            (Json.field "asked" askedDecoder)
                            (Json.field "answered" <|
                                Json.list
                                    (Json.map2 Answered
                                        (Json.field "by" Json.string)
                                        (Json.field "answer" <| Json.oneOf [ Json.null Nothing, Json.map Just Json.string ])
                                    )
                            )
                        )
                      )
                    , (Json.map Accuse
                        (Json.map2 Accusation
                            (Json.field "accused" askedDecoder)
                            (Json.field "answer" Json.bool)
                        )
                      )
                    ]
                )
        )
