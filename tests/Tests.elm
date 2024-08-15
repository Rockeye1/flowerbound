module Tests exposing (bytesRoundtrip, intRoundtrip)

import Bit exposing (Bit(..))
import BitParser
import Bits
import Expect
import Fuzz
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
    Test.fuzz (Fuzz.list (Fuzz.oneOfValues [ I, O ])) "Bit list roundtrips" <|
        \bitList ->
            bitList
                |> BitParser.bitsToBytes
                |> Bits.fromBytes
                |> Expect.equal (bitList ++ List.repeat (modBy 8 -(List.length bitList)) O)
