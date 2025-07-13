module Persona exposing (Colors, default, defaultHue, fromPartial, levelBonus, maxArousal, maxCraving, maxSatiation, maxSensitivity, maxStamina, organColorFromReducedHue, partialGendertropeName, toColors, toPartial, usedEuphoriaPoints, usedIchorPoints, usedNuminousPoints)

import Color.Oklch as Oklch
import Dict
import Persona.Data
import Types exposing (Gendertrope(..), GendertropeRecord, PartialGendertrope(..), PartialPersona, Persona)
import Ui.WithContext exposing (Color)


default : Persona
default =
    { name = "Cinderella Sheen"
    , hue = Nothing

    --
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0

    --
    , gendertrope = Vixen
    , features = []
    }


levelBonus :
    { a
        | fitness : Int
        , grace : Int
        , ardor : Int
        , sanity : Int
        , prowess : Int
        , moxie : Int
        , features : List Int
    }
    -> Int
levelBonus persona =
    let
        highestSum : Int
        highestSum =
            [ persona.fitness
            , persona.grace
            , persona.ardor
            , persona.sanity
            , persona.prowess
            , persona.moxie
            ]
                |> List.sort
                |> List.drop 4
                |> List.sum

        fromAbilityScores : Int
        fromAbilityScores =
            highestSum // 20

        fromFeatures : Int
        fromFeatures =
            -- The +1 is for the implicit level 1 feature
            (List.length persona.features + 1) // 2
    in
    max 1
        (fromAbilityScores + fromFeatures)


intToDots : Int -> String
intToDots i =
    case i of
        0 ->
            "⠀"

        1 ->
            "⠄"

        2 ->
            "⠤"

        3 ->
            "⠦"

        4 ->
            "⠶"

        5 ->
            "⠷"

        _ ->
            "⠷" ++ intToDots (i - 5)


usedIchorPoints : Persona -> Int
usedIchorPoints persona =
    --  TODO
    0


usedEuphoriaPoints : Persona -> Int
usedEuphoriaPoints persona =
    persona.features
        |> List.map (\level -> level + 10)
        |> List.sum


usedNuminousPoints : Persona -> Int
usedNuminousPoints persona =
    max 0
        (persona.fitness
            + persona.grace
            + persona.ardor
            + persona.sanity
            + persona.prowess
            + persona.moxie
            - 18
        )


fromPartial : String -> Maybe PartialPersona -> Maybe GendertropeRecord -> Persona
fromPartial name partialPersona maybeGendertrope =
    let
        defaulted : PartialPersona
        defaulted =
            case partialPersona of
                Just persona ->
                    persona

                Nothing ->
                    toPartial default

        gendertrope : Gendertrope
        gendertrope =
            partialGendertropeToGendertrope defaulted.gendertrope maybeGendertrope
    in
    { name = name
    , hue = defaulted.hue

    --
    , fitness = defaulted.fitness
    , grace = defaulted.grace
    , ardor = defaulted.ardor
    , sanity = defaulted.sanity
    , prowess = defaulted.prowess
    , moxie = defaulted.moxie

    --
    , euphoriaPoints = defaulted.euphoriaPoints
    , ichorPoints = defaulted.ichorPoints
    , numinousPoints = defaulted.numinousPoints

    --
    , features = defaulted.features
    , gendertrope = gendertrope
    }


partialGendertropeToGendertrope : PartialGendertrope -> Maybe GendertropeRecord -> Gendertrope
partialGendertropeToGendertrope gendertrope maybeGendertrope =
    case gendertrope of
        PartialButterfly ->
            Butterfly

        PartialSeedStalker ->
            SeedStalker

        PartialFlower ->
            Flower

        PartialHousepet ->
            Housepet

        PartialVixen ->
            Vixen

        PartialBuck ->
            Buck

        PartialFiend ->
            Fiend

        PartialDoll ->
            Doll
            
        PartialJunglePrince ->
            JunglePrince

        PartialFirecracker ->
            Firecracker

        PartialDemonQueen ->
            DemonQueen

        PartialEldritch ->
            Eldritch

        PartialCustom name ->
            maybeGendertrope
                |> Maybe.withDefault
                    { name = name
                    , description = ""
                    , features = Dict.empty
                    , organs = []
                    , icon = Nothing
                    }
                |> Custom


toPartial : Persona -> PartialPersona
toPartial persona =
    { hue = persona.hue

    --
    , fitness = persona.fitness
    , grace = persona.grace
    , ardor = persona.ardor
    , sanity = persona.sanity
    , prowess = persona.prowess
    , moxie = persona.moxie

    --
    , euphoriaPoints = persona.euphoriaPoints
    , ichorPoints = persona.ichorPoints
    , numinousPoints = persona.numinousPoints

    --
    , features = persona.features
    , gendertrope = gendertropeToPartial persona.gendertrope
    }


gendertropeToPartial : Gendertrope -> PartialGendertrope
gendertropeToPartial gendertrope =
    case gendertrope of
        Butterfly ->
            PartialButterfly

        SeedStalker ->
            PartialSeedStalker

        Flower ->
            PartialFlower

        Housepet ->
            PartialHousepet

        Vixen ->
            PartialVixen

        Buck ->
            PartialBuck

        Fiend ->
            PartialFiend

        Doll ->
            PartialDoll
            
        JunglePrince ->
            PartialJunglePrince 
            
        Firecracker ->
            PartialFirecracker

        DemonQueen ->
            PartialDemonQueen

        Eldritch ->
            PartialEldritch

        Custom { name } ->
            PartialCustom name


partialGendertropeName : Types.PartialGendertrope -> String
partialGendertropeName partial =
    (partialGendertropeToGendertrope partial Nothing
        |> Persona.Data.gendertropeToRecord
    ).name


maxStamina : Persona -> Int
maxStamina _ =
    20


maxSatiation : Persona -> Int
maxSatiation persona =
    20 + 2 * persona.ardor


maxCraving : Persona -> Int
maxCraving persona =
    20 + 2 * persona.sanity


maxSensitivity : Persona -> Int
maxSensitivity persona =
    20 + 2 * persona.moxie


maxArousal : Persona -> Int
maxArousal persona =
    20 + 2 * persona.prowess


type alias Colors =
    { accent : Color
    , background : Color
    , organ : Maybe Color
    }


toColors :
    { a | hue : Maybe Float }
    -> Colors
toColors persona =
    let
        hue : Float
        hue =
            persona.hue
                |> Maybe.withDefault defaultHue

        reducedHue : Float
        reducedHue =
            (hue / 360) - toFloat (floor (hue / 360))
    in
    { accent =
        Oklch.oklch 0.42 0.19 reducedHue
    , background =
        Oklch.oklch 0.98 0.04 reducedHue
    , organ =
        Maybe.map
            (\_ ->
                organColorFromReducedHue reducedHue
            )
            persona.hue
    }


defaultHue : Float
defaultHue =
    328.36341792345144


organColorFromReducedHue : Float -> Color
organColorFromReducedHue reducedHue =
    Oklch.oklch 0.9 0.04 reducedHue
