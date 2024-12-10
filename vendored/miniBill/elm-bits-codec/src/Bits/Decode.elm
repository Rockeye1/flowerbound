module Bits.Decode exposing (Decoder, Error(..), Step(..), andMap, andThen, array, bit, bits, bool, bytes, fail, float, int, list, loop, map, map2, map3, map4, map5, maybe, nonNegativeInt, positiveInt, problem, run, string, succeed)

import Array exposing (Array)
import Bit exposing (Bit)
import Bits
import Bytes exposing (Bytes)
import Bytes.Decode
import List.Extra


type Error e
    = Problem e
    | EndOfInput
    | StringParsingError
    | VariantNotRecognized Int


type Decoder e a
    = Decoder (List Bit -> Result (Error e) ( a, List Bit ))


run : Decoder e a -> List Bit -> Result (Error e) a
run (Decoder dec) input =
    dec input
        |> Result.map Tuple.first


fail : Error e -> Decoder e a
fail e =
    Decoder (\_ -> Err e)


problem : a -> Decoder a b
problem e =
    fail (Problem e)


succeed : a -> Decoder e a
succeed x =
    Decoder (\state -> Ok ( x, state ))


map : (a -> b) -> Decoder x a -> Decoder x b
map f a =
    succeed f
        |> andMap a


map2 : (a -> b -> c) -> Decoder x a -> Decoder x b -> Decoder x c
map2 f a b =
    succeed f
        |> andMap a
        |> andMap b


map3 : (a -> b -> c -> d) -> Decoder x a -> Decoder x b -> Decoder x c -> Decoder x d
map3 f a b c =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c


map4 : (a -> b -> c -> d -> e) -> Decoder x a -> Decoder x b -> Decoder x c -> Decoder x d -> Decoder x e
map4 f a b c d =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


map5 : (a -> b -> c -> d -> e -> f) -> Decoder x a -> Decoder x b -> Decoder x c -> Decoder x d -> Decoder x e -> Decoder x f
map5 f a b c d e =
    succeed f
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


andMap : Decoder e a -> Decoder e (a -> b) -> Decoder e b
andMap (Decoder a) (Decoder f) =
    Decoder
        (\s ->
            case f s of
                Ok ( fv, fs ) ->
                    case a fs of
                        Ok ( av, as_ ) ->
                            Ok ( fv av, as_ )

                        Err e ->
                            Err e

                Err e ->
                    Err e
        )


andThen : (a -> Decoder e b) -> Decoder e a -> Decoder e b
andThen f (Decoder first) =
    Decoder
        (\state ->
            case first state of
                Ok ( x, secondState ) ->
                    let
                        (Decoder second) =
                            f x
                    in
                    second secondState

                Err e ->
                    Err e
        )


type Step state res
    = Loop state
    | Done res


loop : (state -> Decoder e (Step state res)) -> state -> Decoder e res
loop step initial =
    Decoder (loopHelper step initial)


loopHelper : (state -> Decoder e (Step state res)) -> state -> List Bit -> Result (Error e) ( res, List Bit )
loopHelper step acc state =
    let
        (Decoder stepper) =
            step acc
    in
    case stepper state of
        Ok ( Loop newLoopState, newState ) ->
            loopHelper step newLoopState newState

        Ok ( Done v, newState ) ->
            Ok ( v, newState )

        Err e ->
            Err e


bit : Decoder e Bit
bit =
    Decoder
        (\s ->
            case List.Extra.uncons s of
                Nothing ->
                    Err EndOfInput

                Just pair ->
                    Ok pair
        )


bits : Int -> Decoder e (List Bit)
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


bytes : Int -> Decoder e Bytes
bytes width =
    bits (width * 8) |> map Bits.toBytes


int : Decoder e Int
int =
    map
        (\n ->
            if modBy 2 n == 0 then
                n // 2

            else
                -n // 2 - 1
        )
        nonNegativeInt


float : Decoder e Float
float =
    bytes 8
        |> andThen
            (\bs ->
                Decoder
                    (\left ->
                        case Bytes.Decode.decode (Bytes.Decode.float64 Bytes.LE) bs of
                            Just s ->
                                Ok ( s, left )

                            Nothing ->
                                Err StringParsingError
                    )
            )


nonNegativeInt : Decoder e Int
nonNegativeInt =
    map (\n -> n - 1) positiveInt


positiveInt : Decoder e Int
positiveInt =
    loop
        (\n ->
            bit
                |> andThen
                    (\b ->
                        case b of
                            Bit.O ->
                                succeed (Done n)

                            Bit.I ->
                                bits n
                                    |> map (\bs -> Loop (Bits.toIntUnsigned (Bit.I :: bs)))
                    )
        )
        1


array : Decoder e a -> Decoder e (Array a)
array item =
    map Array.fromList (list item)


list : Decoder e a -> Decoder e (List a)
list item =
    nonNegativeInt
        |> andThen
            (\len ->
                loop
                    (\( left, acc ) ->
                        if left <= 0 then
                            succeed (Done (List.reverse acc))

                        else
                            item
                                |> map
                                    (\i ->
                                        Loop ( left - 1, i :: acc )
                                    )
                    )
                    ( len, [] )
            )


maybe : Decoder x a -> Decoder x (Maybe a)
maybe item =
    bit
        |> andThen
            (\b ->
                case b of
                    Bit.O ->
                        succeed Nothing

                    Bit.I ->
                        map Just item
            )


string : Decoder e String
string =
    nonNegativeInt
        |> andThen
            (\width ->
                bytes width
                    |> andThen
                        (\bs ->
                            Decoder
                                (\left ->
                                    case Bytes.Decode.decode (Bytes.Decode.string width) bs of
                                        Just s ->
                                            Ok ( s, left )

                                        Nothing ->
                                            Err StringParsingError
                                )
                        )
            )


bool : Decoder e Bool
bool =
    bit
        |> map
            (\b ->
                case b of
                    Bit.I ->
                        True

                    Bit.O ->
                        False
            )
