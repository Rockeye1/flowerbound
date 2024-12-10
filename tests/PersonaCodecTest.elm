module PersonaCodecTest exposing (persona, personaFuzzer)

import CodecTest
import Dict
import Fuzz exposing (Fuzzer)
import List.Extra
import Persona
import Persona.Codec
import Test exposing (Test, describe)
import Types as Persona exposing (Persona)


persona : Test
persona =
    describe "Persona" [ CodecTest.roundtrips partialPersonaFuzzer Persona.Codec.partialPersona ]


partialPersonaFuzzer : Fuzzer Persona.PartialPersona
partialPersonaFuzzer =
    personaFuzzer
        |> Fuzz.map Persona.toPartial


personaFuzzer : Fuzzer Persona
personaFuzzer =
    Fuzz.constant Persona
        |> Fuzz.andMap tameString
        |> Fuzz.andMap (Fuzz.maybe (Fuzz.floatRange 0 360))
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 2)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.list (Fuzz.intRange 2 5) |> Fuzz.map List.Extra.unique)
        |> Fuzz.andMap gendertropeFuzzer


tameString : Fuzzer String
tameString =
    Fuzz.string
        |> Fuzz.map String.trim
        |> Fuzz.filter
            (\s ->
                not (String.isEmpty s)
                    && not (String.contains "\n" s)
            )


gendertropeFuzzer : Fuzzer Persona.Gendertrope
gendertropeFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Persona.Butterfly
        , Fuzz.constant Persona.Flower
        , Fuzz.constant Persona.Vixen
        , Fuzz.constant Persona.Buck
        , Fuzz.constant Persona.Fiend
        , Fuzz.constant Persona.Doll
        , Fuzz.map Persona.Custom gendertropeRecordFuzzer
        ]


gendertropeRecordFuzzer : Fuzzer Persona.GendertropeRecord
gendertropeRecordFuzzer =
    Fuzz.map5 Persona.GendertropeRecord
        tameString
        tameString
        (Fuzz.list organFuzzer)
        (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.intAtLeast 1) featureFuzzer)))
        (Fuzz.maybe iconFuzzer)


iconFuzzer : Fuzzer { opaque : List String, semitransparent : List String }
iconFuzzer =
    Fuzz.map2
        (\opaque semitransparent ->
            { opaque = opaque
            , semitransparent = semitransparent
            }
        )
        (Fuzz.list tameString)
        (Fuzz.list tameString)
        |> Fuzz.filter
            (\icon ->
                not (List.isEmpty icon.opaque)
                    || not (List.isEmpty icon.semitransparent)
            )


featureFuzzer : Fuzzer Persona.Feature
featureFuzzer =
    Fuzz.map2 Persona.Feature
        tameString
        tameString


organFuzzer : Fuzzer Persona.Organ
organFuzzer =
    Fuzz.constant
        (\name type_ contour erogeny appendages canSquish canGrip canPenetrate canEnsheathe isSquishable isGrippable isPenetrable isEnsheatheable ->
            { name = name
            , type_ = type_
            , contour = contour
            , erogeny = erogeny
            , appendages =
                case type_ of
                    Persona.Hands ->
                        if List.isEmpty appendages then
                            [ emptyAppendage "Left"
                            , emptyAppendage "Right"
                            ]

                        else
                            appendages

                    Persona.Breasts ->
                        if List.isEmpty appendages then
                            [ emptyAppendage "Left"
                            , emptyAppendage "Right"
                            ]

                        else
                            appendages

                    Persona.Hips ->
                        if List.isEmpty appendages then
                            [ emptyAppendage "Fianchetto"
                            ]

                        else
                            appendages

                    Persona.Legs ->
                        if List.isEmpty appendages then
                            [ emptyAppendage "Left"
                            , emptyAppendage "Right"
                            ]

                        else
                            appendages

                    _ ->
                        appendages
            , canSquish = canSquish
            , canGrip = canGrip
            , canPenetrate = canPenetrate
            , canEnsheathe = canEnsheathe
            , isSquishable = isSquishable
            , isGrippable = isGrippable
            , isPenetrable = isPenetrable
            , isEnsheatheable = isEnsheatheable
            }
        )
        |> Fuzz.andMap tameString
        |> Fuzz.andMap organTypeFuzzer
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.list appendageFuzzer)
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool


emptyAppendage : String -> Persona.Appendage
emptyAppendage name =
    { name = name
    , canSquish = False
    , canGrip = False
    , canPenetrate = False
    , canEnsheathe = False
    , isSquishable = False
    , isGrippable = False
    , isPenetrable = False
    , isEnsheatheable = False
    }


appendageFuzzer : Fuzzer Persona.Appendage
appendageFuzzer =
    Fuzz.constant Persona.Appendage
        |> Fuzz.andMap tameString
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool


organTypeFuzzer : Fuzzer Persona.OrganType
organTypeFuzzer =
    Fuzz.oneOfValues
        [ Persona.Mouth
        , Persona.Hands
        , Persona.Breasts
        , Persona.Hips
        , Persona.Yonic
        , Persona.Phallic
        , Persona.Legs
        , Persona.Prehensile
        , Persona.Other
        ]
