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



-- Json decoders/encoders ----------------------------------------------------------------------------------------------


personDecoder : Json.Decoder Person
personDecoder =
    let
        decodeToType string =
            case string of
                "ColMustard" ->
                    Result.Ok ColMustard

                "MrsWhite" ->
                    Result.Ok MrsWhite

                "MsPeacock" ->
                    Result.Ok MsPeacock

                "MsScarlett" ->
                    Result.Ok MsScarlett

                "ProfPlum" ->
                    Result.Ok ProfPlum

                "RevGreen" ->
                    Result.Ok RevGreen

                _ ->
                    Result.Err ("Not valid pattern for decoder to Person. Pattern: " ++ (toString string))
    in
        Json.customDecoder Json.string decodeToType


weaponDecoder : Json.Decoder Weapon
weaponDecoder =
    let
        decodeToType string =
            case string of
                "Candlestick" ->
                    Result.Ok Candlestick

                "IcePick" ->
                    Result.Ok IcePick

                "Poison" ->
                    Result.Ok Poison

                "Poker" ->
                    Result.Ok Poker

                "Revolver" ->
                    Result.Ok Revolver

                "Shears" ->
                    Result.Ok Shears

                _ ->
                    Result.Err ("Not valid pattern for decoder to Weapon. Pattern: " ++ (toString string))
    in
        Json.customDecoder Json.string decodeToType


locationDecoder : Json.Decoder Location
locationDecoder =
    let
        decodeToType string =
            case string of
                "BedRoom" ->
                    Result.Ok BedRoom

                "Billiards" ->
                    Result.Ok Billiards

                "Conservatory" ->
                    Result.Ok Conservatory

                "Kitchen" ->
                    Result.Ok Kitchen

                "Library" ->
                    Result.Ok Library

                "Lounge" ->
                    Result.Ok Lounge

                "Stairs" ->
                    Result.Ok Stairs

                "Studio" ->
                    Result.Ok Studio

                "TrophyHall" ->
                    Result.Ok TrophyHall

                _ ->
                    Result.Err ("Not valid pattern for decoder to Weapon. Pattern: " ++ (toString string))
    in
        Json.customDecoder Json.string decodeToType
