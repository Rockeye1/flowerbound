module Tests exposing (bytesRoundtrip, intRoundtrip)

import Bit exposing (Bit(..))
import BitParser
import Bits
import Bytes
import Bytes.Encode
import Expect
import Fuzz
import Route.Persona.Name_.Data__
import Test exposing (Test)


intRoundtrip : Test
intRoundtrip =
    Test.fuzz (Fuzz.intAtLeast 1) "Int roundtrips" <|
        \i ->
            let
                encoded =
                    i
                        |> Route.Persona.Name_.Data__.encodePositiveInt
                        |> Debug.log "Encoded as"
            in
            case BitParser.run Route.Persona.Name_.Data__.parsePositiveInt encoded of
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
