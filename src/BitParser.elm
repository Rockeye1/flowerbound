module BitParser exposing (Parser, Step(..), andMap, andThen, bit, bits, bitsToBytes, fail, loop, map, run, stringDecoder, succeed)

import Bit exposing (Bit(..))
import Bits
import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import List.Extra


type Parser result
    = Parser (State -> Maybe ( result, State ))


type alias State =
    List Bit


run : Parser result -> List Bit -> Maybe result
run (Parser p) input =
    Maybe.map Tuple.first (p input)


bit : Parser Bit
bit =
    Parser List.Extra.uncons


type Step state res
    = Loop state
    | Done res


loop : (state -> Parser (Step state res)) -> state -> Parser res
loop step initial =
    Parser (loopHelper step initial)


loopHelper : (state -> Parser (Step state res)) -> state -> State -> Maybe ( res, State )
loopHelper step acc state =
    let
        (Parser stepper) =
            step acc
    in
    case stepper state of
        Just ( Loop newLoopState, newState ) ->
            loopHelper step newLoopState newState

        Just ( Done v, newState ) ->
            Just ( v, newState )

        Nothing ->
            Nothing


fail : Parser a
fail =
    Parser (\_ -> Nothing)


succeed : a -> Parser a
succeed x =
    Parser (\state -> Just ( x, state ))


andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f (Parser first) =
    Parser
        (\state ->
            first state
                |> Maybe.andThen
                    (\( x, secondState ) ->
                        let
                            (Parser second) =
                                f x
                        in
                        second secondState
                    )
        )


bits : Int -> Parser (List Bit)
bits width =
    loop
        (\( left, acc ) ->
            if left <= 0 then
                succeed (Done (List.reverse acc))

            else
                bit
                    |> map (\b -> Loop ( left - 1, b :: acc ))
        )
        ( width, [] )


map : (a -> b) -> Parser a -> Parser b
map f p =
    p |> andThen (\x -> succeed (f x))


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap second first =
    first |> andThen (\f -> second |> map (\s -> f s))


stringDecoder : Int -> Parser String
stringDecoder width =
    bits (width * 8)
        |> andThen
            (\bs ->
                case
                    bs
                        |> bitsToBytes
                        |> Bytes.Decode.decode (Bytes.Decode.string width)
                of
                    Just s ->
                        succeed s

                    Nothing ->
                        fail
            )


bitsToBytes : List Bit -> Bytes
bitsToBytes bs =
    let
        length : Int
        length =
            List.length bs

        padded : List Bit
        padded =
            bs ++ List.repeat (modBy 8 -length) O
    in
    Bits.toIntUnsigned8s padded
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode
