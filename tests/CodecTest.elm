module CodecTest exposing (roundtrips, suite)

import Bit
import Bits.Codec as Codec exposing (Codec)
import Bits.Decode
import Dict
import Expect
import Fuzz exposing (Fuzzer)
import Rope
import Test exposing (Test, describe, fuzz, fuzz2, test)


suite : Test
suite =
    describe "Testing roundtrips"
        [ describe "Basic" basicTests
        , describe "Containers" containersTests
        , describe "Object" objectTests
        , describe "Custom" customTests

        -- , describe "bimap" bimapTests
        -- , describe "maybe" maybeTests
        -- , describe "nullable" nullableTests
        , describe "succeed"
            [ test "roundtrips"
                (\_ ->
                    let
                        codec : Codec e number
                        codec =
                            Codec.succeed 632
                    in
                    Bits.Decode.run codec.decoder []
                        |> Expect.equal (Ok 632)
                )
            ]

        -- , describe "recursive" recursiveTests
        , describe "map,andThen" mapAndThenTests
        ]


roundtrips : Fuzzer a -> Codec e a -> Test
roundtrips fuzzer codec =
    fuzz2 fuzzer (Fuzz.list (Fuzz.oneOfValues [ Bit.I, Bit.O ])) "is a roundtrip" <|
        \value tail ->
            Expect.all
                [ \_ ->
                    codec.encoder value
                        |> Rope.toList
                        |> Bits.Decode.run codec.decoder
                        |> Expect.equal (Ok value)
                , \_ ->
                    codec.encoder value
                        |> Rope.toList
                        -- Check that random additional bits are ignored
                        |> (\l -> l ++ tail)
                        |> Bits.Decode.run codec.decoder
                        |> Expect.equal (Ok value)
                ]
                ()


roundtripsWithin : Fuzzer Float -> Codec e Float -> Test
roundtripsWithin fuzzer codec =
    fuzz fuzzer "is a roundtrip" <|
        \value ->
            case
                codec.encoder value
                    |> Rope.toList
                    |> Bits.Decode.run codec.decoder
            of
                Err _ ->
                    Expect.fail "Decoding failed"

                Ok v ->
                    if isNaN value then
                        if isNaN v then
                            Expect.pass

                        else
                            Expect.fail "NaN decoded to non-NaN"

                    else
                        v
                            |> Expect.within (Expect.Relative 0.000001) value


basicTests : List Test
basicTests =
    [ describe "Codec.string"
        [ roundtrips Fuzz.string Codec.string
        ]
    , describe "Codec.int"
        [ roundtrips Fuzz.int Codec.int
        ]
    , describe "Codec.float"
        [ roundtrips
            (Fuzz.oneOf
                [ Fuzz.niceFloat
                , Fuzz.constant (1 / 0)
                , Fuzz.constant (-1 / 0)
                ]
            )
            Codec.float
        , describe "Works for NaN"
            [ roundtripsWithin Fuzz.float Codec.float
            ]
        ]
    , describe "Codec.bool"
        [ roundtrips Fuzz.bool Codec.bool
        ]
    ]


containersTests : List Test
containersTests =
    [ describe "Codec.array"
        [ roundtrips (Fuzz.array Fuzz.int) (Codec.array Codec.int)
        ]
    , describe "Codec.list"
        [ roundtrips (Fuzz.list Fuzz.int) (Codec.list Codec.int)
        ]
    , describe "Codec.dict"
        [ roundtrips
            (Fuzz.map2 Tuple.pair Fuzz.string Fuzz.int
                |> Fuzz.list
                |> Fuzz.map Dict.fromList
            )
            (Codec.dict Codec.string Codec.int)
        ]

    -- , describe "Codec.set"
    --     [ roundtrips
    --         (Fuzz.list Fuzz.int |> Fuzz.map Set.fromList)
    --         (Codec.set Codec.int)
    --     ]
    , describe "Codec.tuple"
        [ roundtrips
            (Fuzz.pair Fuzz.int Fuzz.int)
            (Codec.tuple Codec.int Codec.int)
        ]
    ]


objectTests : List Test
objectTests =
    [ describe "with 0 fields"
        [ roundtrips (Fuzz.constant {})
            (Codec.object {}
                |> Codec.buildObject
            )
        ]
    , describe "with 1 field"
        [ roundtrips (Fuzz.map (\i -> { fname = i }) Fuzz.int)
            (Codec.object (\i -> { fname = i })
                |> Codec.field .fname Codec.int
                |> Codec.buildObject
            )
        ]
    , describe "with 2 fields"
        [ roundtrips
            (Fuzz.map2
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                Fuzz.int
                Fuzz.int
            )
            (Codec.object
                (\a b ->
                    { a = a
                    , b = b
                    }
                )
                |> Codec.field .a Codec.int
                |> Codec.field .b Codec.int
                |> Codec.buildObject
            )
        ]
    ]


type Empty
    = Empty


type Newtype a
    = Newtype a


customTests : List Test
customTests =
    [ describe "with 1 ctor, 0 args"
        [ roundtrips (Fuzz.constant Empty)
            (Codec.custom
                (\f v ->
                    case v of
                        Empty ->
                            f
                )
                |> Codec.variant0 Empty
                |> Codec.buildCustom
            )
        ]
    , describe "with 1 ctor, 1 arg"
        [ roundtrips (Fuzz.map Newtype Fuzz.int)
            (Codec.custom
                (\f v ->
                    case v of
                        Newtype a ->
                            f a
                )
                |> Codec.variant1 Newtype Codec.int
                |> Codec.buildCustom
            )
        ]
    , describe "with 2 ctors, 0,1 args" <|
        let
            match : b -> (a -> b) -> Maybe a -> b
            match fnothing fjust value =
                case value of
                    Nothing ->
                        fnothing

                    Just v ->
                        fjust v

            codec : Codec e (Maybe Int)
            codec =
                Codec.custom match
                    |> Codec.variant0 Nothing
                    |> Codec.variant1 Just Codec.int
                    |> Codec.buildCustom

            fuzzers : List ( String, Fuzzer (Maybe Int) )
            fuzzers =
                [ ( "1st ctor", Fuzz.constant Nothing )
                , ( "2nd ctor", Fuzz.map Just Fuzz.int )
                ]
        in
        fuzzers
            |> List.map
                (\( name, fuzz ) ->
                    describe name
                        [ roundtrips fuzz codec ]
                )
    ]



-- bimapTests : List Test
-- bimapTests =
--     [ roundtripsWithin Fuzz.float <|
--         Codec.map
--             (\x -> x * 2)
--             (\x -> x / 2)
--             Codec.float
--     ]
-- maybeTests : List Test
-- maybeTests =
--     [ describe "single"
--         [ roundtrips
--             (Fuzz.oneOf
--                 [ Fuzz.constant Nothing
--                 , Fuzz.map Just Fuzz.int
--                 ]
--             )
--           <|
--             Codec.maybe Codec.int
--         ]
--     , test "Decodes wrong types to Nothing" <|
--         \_ ->
--             "3"
--                 |> Codec.decodeString (Codec.maybe Codec.string)
--                 |> Expect.equal (Ok Nothing)
--     {-
--        This is a known limitation: using null as Nothing and identity as Just means that nesting two maybes squashes Just Nothing with Nothing
--        , describe "double"
--           [ roundtrips
--               (Fuzz.oneOf
--                   [ Fuzz.constant Nothing
--                   , Fuzz.constant <| Just Nothing
--                   , Fuzz.map (Just << Just) Fuzz.int
--                   ]
--               )
--             <|
--               Codec.maybe <|
--                   Codec.maybe Codec.int
--           ]
--     -}
--     ]
-- nullableTests : List Test
-- nullableTests =
--     [ describe "single"
--         [ roundtrips
--             (Fuzz.oneOf
--                 [ Fuzz.constant Nothing
--                 , Fuzz.map Just Fuzz.int
--                 ]
--             )
--           <|
--             Codec.nullable Codec.int
--         ]
--     , test "Does not decode wrong types" <|
--         \_ ->
--             "3"
--                 |> Codec.decodeString (Codec.nullable Codec.string)
--                 |> Expect.notEqual (Ok Nothing)
--     ]
-- recursiveTests : List Test
-- recursiveTests =
--     [ describe "list"
--         [ roundtrips (Fuzz.list Fuzz.int) <|
--             Codec.recursive
--                 (\c ->
--                     Codec.custom
--                         (\fempty fcons value ->
--                             case value of
--                                 [] ->
--                                     fempty
--                                 x :: xs ->
--                                     fcons x xs
--                         )
--                         |> Codec.variant0 "[]" []
--                         |> Codec.variant2 "(::)" (::) Codec.int c
--                         |> Codec.buildCustom
--                 )
--         ]
--     ]


mapAndThenTests : List Test
mapAndThenTests =
    [ describe "Codec.map"
        [ roundtrips (Fuzz.intRange -10000 10000) <|
            Codec.map (\x -> x - 1) (\x -> x + 1) Codec.int
        ]
    ]
