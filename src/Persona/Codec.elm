module Persona.Codec exposing (gendertropeRecord, partialPersona)

import Bits.Codec as Codec exposing (Codec)
import Persona.Types exposing (Feature, GendertropeRecord, Organ, PartialGendertrope(..), PartialPersona)


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
        |> Codec.field .features (Codec.dict Codec.positiveInt feature)
        |> Codec.field .organs (Codec.list organ)
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
