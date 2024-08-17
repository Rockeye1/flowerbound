module PersonaCodecTest exposing (persona)

import CodecTest
import Dict
import Fuzz exposing (Fuzzer)
import Persona
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
    Fuzz.constant
        { default
            | gendertrope =
                Persona.Custom
                    { name = "Custom"
                    , description = "..."
                    , features = Dict.empty
                    , organs = []
                    }
        }
