module Persona.Codec exposing (gendertropeRecord, partialPersona, personaParser)

import Bits.Codec as Codec exposing (Codec)
import Dict
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Persona.Data as Data
import Persona.Types exposing (Feature, Gendertrope(..), GendertropeRecord, Organ, PartialGendertrope(..), PartialPersona, Persona)


personaParser : Parser Persona
personaParser =
    Parser.succeed Persona
        |= headerParser 1 (Parser.getChompedString (Parser.Workaround.chompUntilBefore "\n"))
        |. headerParser 2 (Parser.keyword "Ability Scores")
        |= ulParser "Fitness" Parser.int
        |= ulParser "Grace" Parser.int
        |= ulParser "Ardor" Parser.int
        |= ulParser "Sanity" Parser.int
        |= ulParser "Prowess" Parser.int
        |= ulParser "Moxie" Parser.int
        |. headerParser 2 (Parser.keyword "Progression Tally")
        |= ulParser "Euphoria Points" Parser.int
        |= ulParser "Ichor Points" Parser.int
        |= ulParser "Numinous Points" Parser.int
        |. headerParser 2 (Parser.keyword "Unlocked features")
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = "\n"
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.symbol "-"
                    |. Parser.spaces
                    |. Parser.keyword "Level"
                    |. Parser.spaces
                    |= Parser.int
            , trailing = Parser.Optional
            }
        |. Parser.spaces
        |= gendertropeParser
        |. Parser.end


gendertropeParser : Parser Gendertrope
gendertropeParser =
    let
        nameParser : Parser String
        nameParser =
            Parser.succeed identity
                |. Parser.symbol "##"
                |. Parser.spaces
                |. Parser.keyword "Gendertrope:"
                |. Parser.spaces
                |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "\n")
                |. Parser.spaces
    in
    nameParser
        |> Parser.andThen
            (\name ->
                case name of
                    "The Butterfly" ->
                        Parser.succeed Butterfly

                    "The Flower" ->
                        Parser.succeed Flower

                    "The Vixen" ->
                        Parser.succeed Vixen

                    "The Buck" ->
                        Parser.succeed Buck

                    "The Fiend" ->
                        Parser.succeed Fiend

                    "The Doll" ->
                        Parser.succeed Doll

                    _ ->
                        Parser.map Custom (gendertropeRecordParser name)
            )


gendertropeRecordParser : String -> Parser GendertropeRecord
gendertropeRecordParser name =
    Parser.succeed (GendertropeRecord name)
        |= Parser.map String.trim (Parser.getChompedString (Parser.Workaround.chompUntilBefore "###"))
        |. headerParser 3 (Parser.keyword "Organs")
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.symbol "-"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed Data.mouth |. Parser.keyword "Mouth"
                        , Parser.succeed Data.hands |. Parser.keyword "Hands"
                        , Parser.succeed Data.breasts |. Parser.keyword "Breasts"
                        , Parser.succeed Data.hips |. Parser.keyword "Hips"
                        , Parser.succeed Data.legs |. Parser.keyword "Legs"
                        , Parser.succeed Data.phallic |. Parser.keyword "Phallic"
                        , Parser.succeed Data.yonic |. Parser.keyword "Yonic"
                        , Parser.succeed Data.prehensile |. Parser.keyword "Prehensile"
                        ]
                    |. Parser.spaces
                    |. Parser.symbol ":"
                    |. Parser.spaces
                    |= Parser.getChompedString (Parser.Workaround.chompUntilBefore "\n")
                    |. Parser.spaces
            , trailing = Parser.Optional
            }
        |= (Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = Parser.spaces
                , item =
                    Parser.succeed Tuple.pair
                        |= headerParser 3
                            (Parser.succeed identity
                                |. Parser.keyword "Level"
                                |. Parser.spaces
                                |= Parser.int
                                |. Parser.spaces
                                |. Parser.keyword "Feature"
                                |. Parser.spaces
                                |. Parser.symbol ":"
                            )
                        |= featureParser
                , trailing = Parser.Optional
                }
                |> Parser.map Dict.fromList
           )


featureParser : Parser Feature
featureParser =
    Parser.succeed Feature
        |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "\n")
        |. Parser.spaces
        |= Parser.map String.trim (Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "###"))


headerParser : Int -> Parser a -> Parser a
headerParser level inner =
    let
        hashes : String
        hashes =
            String.repeat level "#"
    in
    Parser.succeed identity
        |. Parser.symbol hashes
        |. Parser.spaces
        |= inner
        |. Parser.spaces


ulParser : String -> Parser a -> Parser a
ulParser name inner =
    Parser.succeed identity
        |. Parser.symbol "-"
        |. Parser.spaces
        |. Parser.keyword name
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces
        |= inner
        |. Parser.spaces


partialPersona : Codec e PartialPersona
partialPersona =
    Codec.object PartialPersona
        |> Codec.field .fitness (Codec.intWithMinimum 2)
        |> Codec.field .grace (Codec.intWithMinimum 2)
        |> Codec.field .ardor (Codec.intWithMinimum 2)
        |> Codec.field .sanity (Codec.intWithMinimum 2)
        |> Codec.field .prowess (Codec.intWithMinimum 2)
        |> Codec.field .moxie (Codec.intWithMinimum 2)
        |> Codec.field .euphoriaPoints Codec.nonNegativeInt
        |> Codec.field .ichorPoints Codec.nonNegativeInt
        |> Codec.field .numinousPoints Codec.nonNegativeInt
        |> Codec.field .features (Codec.list (Codec.intWithMinimum 2))
        |> Codec.field .gendertrope partialGendertrope
        |> Codec.buildObject


feature : Codec e Feature
feature =
    Codec.object Feature
        |> Codec.field .name Codec.string
        |> Codec.field .description Codec.string
        |> Codec.buildObject


partialGendertrope : Codec e PartialGendertrope
partialGendertrope =
    Codec.custom
        (\fCustom fButterfly fFlower fVixen fBuck fFiend fDoll value ->
            case value of
                PartialCustom name ->
                    fCustom name

                PartialButterfly ->
                    fButterfly

                PartialFlower ->
                    fFlower

                PartialVixen ->
                    fVixen

                PartialBuck ->
                    fBuck

                PartialFiend ->
                    fFiend

                PartialDoll ->
                    fDoll
        )
        |> Codec.variant1 PartialCustom Codec.string
        |> Codec.variant0 PartialButterfly
        |> Codec.variant0 PartialFlower
        |> Codec.variant0 PartialVixen
        |> Codec.variant0 PartialBuck
        |> Codec.variant0 PartialFiend
        |> Codec.variant0 PartialDoll
        |> Codec.buildCustom


gendertropeRecord : Codec e GendertropeRecord
gendertropeRecord =
    Codec.object GendertropeRecord
        |> Codec.field .name Codec.string
        |> Codec.field .description Codec.string
        |> Codec.field .organs (Codec.list organ)
        |> Codec.field .features (Codec.dict Codec.positiveInt feature)
        |> Codec.buildObject


organ : Codec e Organ
organ =
    Codec.object Organ
        |> Codec.field .name Codec.string
        |> Codec.field .contour Codec.nonNegativeInt
        |> Codec.field .erogeny Codec.nonNegativeInt
        |> Codec.field .canSquish Codec.bool
        |> Codec.field .canGrip Codec.bool
        |> Codec.field .canPenetrate Codec.bool
        |> Codec.field .canEnsheathe Codec.bool
        |> Codec.field .isSquishable Codec.bool
        |> Codec.field .isGrippable Codec.bool
        |> Codec.field .isPenetrable Codec.bool
        |> Codec.field .isEnsheatheable Codec.bool
        |> Codec.buildObject
