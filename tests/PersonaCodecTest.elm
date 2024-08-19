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
    Fuzz.map4 Persona.GendertropeRecord
        tameString
        tameString
        (Fuzz.list organFuzzer)
        (Fuzz.map Dict.fromList (Fuzz.list (Fuzz.pair (Fuzz.intAtLeast 1) featureFuzzer)))


featureFuzzer : Fuzzer Persona.Feature
featureFuzzer =
    Fuzz.map2 Persona.Feature
        tameString
        tameString


organFuzzer : Fuzzer Persona.Organ
organFuzzer =
    Fuzz.constant Persona.Organ
        |> Fuzz.andMap tameString
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
