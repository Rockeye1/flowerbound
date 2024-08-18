module PersonaCodecTest exposing (persona)

import CodecTest
import Dict
import Fuzz exposing (Fuzzer)
import Persona
import Persona.Types as Persona
import Test exposing (Test, describe)


persona : Test
persona =
    describe "Persona" [ CodecTest.roundtrips partialPersonaFuzzer Persona.codec ]


partialPersonaFuzzer : Fuzzer Persona.PartialPersona
partialPersonaFuzzer =
    let
        default : Persona.PartialPersona
        default =
            Persona.default
                |> Persona.toPartial
    in
    Fuzz.map
        (\gendertrope ->
            { default
                | gendertrope = Persona.gendertropeToPartial gendertrope
            }
        )
        gendertropeFuzzer


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
    Fuzz.map4 Persona.GendertropeRecord
        Fuzz.string
        Fuzz.string
        (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.intAtLeast 1) featureFuzzer)))
        (Fuzz.list organFuzzer)


featureFuzzer : Fuzzer Persona.Feature
featureFuzzer =
    Fuzz.map2 Persona.Feature
        Fuzz.string
        Fuzz.string


organFuzzer : Fuzzer Persona.Organ
organFuzzer =
    Fuzz.constant Persona.Organ
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap (Fuzz.intAtLeast 0)
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
        |> Fuzz.andMap Fuzz.bool
