module MarkdownRoundtripTest exposing (fuzzy, simple)

import Dict
import Expect
import Parser
import Persona
import Persona.Codec
import PersonaCodecTest
import Test exposing (Test, fuzz, test)
import Types exposing (Gendertrope(..), Persona)


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
                    , hue = Nothing
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
                    , hue = Nothing
                    }
        , test "Multiple features" <|
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
                    , hue = Just 3.14
                    }
        , test "Empty appendages" <|
            \_ ->
                roundtrips True
                    { ardor = 2
                    , euphoriaPoints = 0
                    , features = []
                    , fitness = 2
                    , gendertrope =
                        Custom
                            { description = "!"
                            , features = Dict.empty
                            , icon = Nothing
                            , name = "!"
                            , organs =
                                [ { appendages = []
                                  , canEnsheathe = False
                                  , canGrip = False
                                  , canPenetrate = False
                                  , canSquish = False
                                  , contour = 0
                                  , erogeny = 0
                                  , isEnsheatheable = False
                                  , isGrippable = False
                                  , isPenetrable = False
                                  , isSquishable = False
                                  , name = "!"
                                  , type_ = Types.Hands
                                  }
                                ]
                            }
                    , grace = 2
                    , hue = Nothing
                    , ichorPoints = 0
                    , moxie = 2
                    , name = "!"
                    , numinousPoints = 0
                    , prowess = 2
                    , sanity = 2
                    }
        , test "Default" <|
            \_ ->
                roundtrips True
                    Persona.default
        ]


fuzzy : Test
fuzzy =
    fuzz PersonaCodecTest.personaFuzzer
        "A Persona roundtrips through Markdown"
        (roundtrips False)


roundtrips : Bool -> Persona -> Expect.Expectation
roundtrips log persona =
    let
        markdown : String
        markdown =
            Persona.Codec.toString persona

        _ =
            if log then
                Debug.log "Markdown" markdown

            else
                ""
    in
    case Parser.run Persona.Codec.personaParser markdown of
        Err e ->
            Expect.fail (Debug.toString e)

        Ok actual ->
            actual
                |> Expect.equal persona
