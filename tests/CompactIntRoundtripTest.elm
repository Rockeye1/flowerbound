module CompactIntRoundtripTest exposing (bytesRoundtrip, compressRoundtrip, intRoundtrip, nonNegativeIntRoudtrips, positiveIntRoundtrip)

import Bit exposing (Bit(..))
import Bits
import Bits.Decode
import Bits.Encode
import Expect
import Fuzz
import Persona.Codec
import Rope
import Test exposing (Test)


intRoundtrip : Test
intRoundtrip =
    Test.fuzz Fuzz.int "Int roundtrips" <|
        \i ->
            let
                encoded : List Bit
                encoded =
                    i
                        |> Bits.Encode.int
                        |> Rope.toList
            in
            case Bits.Decode.run Bits.Decode.int encoded of
                Err e ->
                    Expect.fail ("Could not roundtrip: " ++ Debug.toString e)

                Ok w ->
                    w
                        |> Expect.equal i


nonNegativeIntRoudtrips : Test
nonNegativeIntRoudtrips =
    Test.fuzz (Fuzz.intAtLeast 0) "Non negative Int roundtrips" <|
        \i ->
            let
                encoded : List Bit
                encoded =
                    i
                        |> Bits.Encode.nonNegativeInt
                        |> Rope.toList
            in
            case Bits.Decode.run Bits.Decode.nonNegativeInt encoded of
                Err e ->
                    Expect.fail ("Could not roundtrip: " ++ Debug.toString e)

                Ok w ->
                    w
                        |> Expect.equal i


positiveIntRoundtrip : Test
positiveIntRoundtrip =
    Test.fuzz (Fuzz.intAtLeast 1) "Positive Int roundtrips" <|
        \i ->
            let
                encoded : List Bit
                encoded =
                    i
                        |> Bits.Encode.positiveInt
                        |> Rope.toList
            in
            case Bits.Decode.run Bits.Decode.positiveInt encoded of
                Err e ->
                    Expect.fail ("Could not roundtrip: " ++ Debug.toString e)

                Ok w ->
                    w
                        |> Expect.equal i


bytesRoundtrip : Test
bytesRoundtrip =
    Test.fuzz (Fuzz.list bitFuzzer) "Bit list roundtrips" <|
        \bitList ->
            bitList
                |> Bits.toBytes
                |> Bits.fromBytes
                |> Expect.equal (bitList ++ List.repeat (modBy 8 -(List.length bitList)) O)


bitFuzzer : Fuzz.Fuzzer Bit
bitFuzzer =
    Fuzz.oneOfValues [ I, O ]


compressRoundtrip : Test
compressRoundtrip =
    Test.fuzz (Fuzz.listOfLengthBetween 0 (8 * 1024) bitFuzzer) "maybeCompress roundtrips" <|
        \bits ->
            let
                actual : List Bit
                actual =
                    bits
                        |> Bits.toBytes
                        |> Persona.Codec.maybeCompress
                        |> Bits.fromBytes
                        |> Persona.Codec.maybeDecompress
                        |> Maybe.withDefault []
            in
            actual
                |> List.take (List.length bits)
                |> Expect.equalLists bits
