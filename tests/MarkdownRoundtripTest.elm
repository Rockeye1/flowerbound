module MarkdownRoundtripTest exposing (roundtrip, simple)

import Dict
import Expect
import Parser
import Persona.Codec
import Persona.Types exposing (Gendertrope(..), Persona)
import PersonaCodecTest
import Test exposing (Test, fuzz, test)


simple : Test
simple =
    Test.describe "Simple"
        [ test "Minimal" <|
            \_ ->
                roundtrips True
                    { ardor = 2
                    , euphoriaPoints = 0
                    , features = []
                    , fitness = 2
                    , gendertrope = Butterfly
                    , grace = 2
                    , ichorPoints = 0
                    , moxie = 2
                    , name = "A"
                    , numinousPoints = 0
                    , prowess = 2
                    , sanity = 2
                    }
        , test "Negative points" <|
            \_ ->
                roundtrips True
                    { ardor = 2
                    , euphoriaPoints = 0
                    , features = []
                    , fitness = 9
                    , gendertrope = Butterfly
                    , grace = 2
                    , ichorPoints = 0
                    , moxie = 2
                    , name = "!"
                    , numinousPoints = 0
                    , prowess = 2
                    , sanity = 2
                    }
        , test "Bang" <|
            \_ ->
                roundtrips True
                    { ardor = 2
                    , euphoriaPoints = 0
                    , features = [ 2, 3 ]
                    , fitness = 2
                    , gendertrope = Butterfly
                    , grace = 2
                    , ichorPoints = 0
                    , moxie = 2
                    , name = "!"
                    , numinousPoints = 0
                    , prowess = 2
                    , sanity = 2
                    }
        ]


roundtrip : Test
roundtrip =
    fuzz PersonaCodecTest.personaFuzzer
        "A Persona roundtrips through Markdown"
        (roundtrips False)


roundtrips : Bool -> Persona -> Expect.Expectation
roundtrips log persona =
    let
        markdown : String
        markdown =
            Persona.Codec.toString persona
    in
    case Parser.run Persona.Codec.personaParser markdown of
        Err e ->
            let
                _ =
                    if log then
                        Debug.log "Markdown" markdown

                    else
                        ""
            in
            Expect.fail (Debug.toString e)

        Ok actual ->
            actual
                |> Expect.equal persona
