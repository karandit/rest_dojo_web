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
