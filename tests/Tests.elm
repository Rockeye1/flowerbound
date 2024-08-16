module Tests exposing (bytesRoundtrip, compressRoundtrip, intRoundtrip)

import Bit exposing (Bit(..))
import BitParser
import Bits
import Expect
import Fuzz
import Route.Persona.Name_.Data__
import Test exposing (Test)


intRoundtrip : Test
intRoundtrip =
    Test.fuzz (Fuzz.intAtLeast 1) "Int roundtrips" <|
        \i ->
            let
                encoded : List Bit
                encoded =
                    i
                        |> BitParser.encodePositiveInt
            in
            case BitParser.run BitParser.parsePositiveInt encoded of
                Nothing ->
                    Expect.fail "Could not roundtrip"

                Just w ->
                    w
                        |> Expect.equal i


bytesRoundtrip : Test
bytesRoundtrip =
    Test.fuzz (Fuzz.list bitFuzzer) "Bit list roundtrips" <|
        \bitList ->
            bitList
                |> BitParser.bitsToBytes
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
                        |> BitParser.bitsToBytes
                        |> Route.Persona.Name_.Data__.maybeCompress
                        |> Bits.fromBytes
                        |> Route.Persona.Name_.Data__.maybeDecompress
            in
            actual
                |> List.take (List.length bits)
                |> Expect.equalLists bits
