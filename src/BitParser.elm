module BitParser exposing (Parser, Step(..), andMap, andThen, bitsToBytes, encodeInt, encodeList, encodeNonnegativeInt, encodePositiveInt, encodeString, fail, loop, map, map2, map3, parseInt, parseList, parseNonnegativeInt, parsePositiveInt, parseString, run, succeed)

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


bytes : Int -> Parser Bytes
bytes width =
    bits (width * 8) |> map bitsToBytes


map : (a -> b) -> Parser a -> Parser b
map f p =
    p |> andThen (\x -> succeed (f x))


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 f l r =
    l |> andThen (\x -> r |> andThen (\y -> succeed (f x y)))


map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 f a b c =
    a |> andThen (\va -> b |> andThen (\vb -> c |> andThen (\vc -> succeed (f va vb vc))))


andMap : Parser a -> Parser (a -> b) -> Parser b
andMap second first =
    first |> andThen (\f -> second |> map (\s -> f s))


bitsToBytes : List Bit -> Bytes
bitsToBytes bs =
    bs
        |> Bits.toIntUnsigned8s
        |> List.map Bytes.Encode.unsignedInt8
        |> Bytes.Encode.sequence
        |> Bytes.Encode.encode


encodeInt : Int -> List Bit
encodeInt n =
    encodeNonnegativeInt
        (if n >= 0 then
            2 * n

         else
            -2 * n - 1
        )


encodeNonnegativeInt : Int -> List Bit
encodeNonnegativeInt n =
    encodePositiveInt (n + 1)


encodePositiveInt : Int -> List Bit
encodePositiveInt i =
    if i < 1 then
        []

    else
        encodePositiveIntHelper i [ O ]


encodePositiveIntHelper : Int -> List Bit -> List Bit
encodePositiveIntHelper n acc =
    if n == 1 then
        acc

    else
        let
            length : Int
            length =
                ceiling (logBase 2 (toFloat n + 1))
        in
        encodePositiveIntHelper (length - 1) (Bits.fromIntUnsigned length n ++ acc)


parseInt : Parser Int
parseInt =
    map
        (\n ->
            if modBy 2 n == 0 then
                n // 2

            else
                -n // 2
        )
        parseNonnegativeInt


parseNonnegativeInt : Parser Int
parseNonnegativeInt =
    map (\n -> n - 1) parsePositiveInt


parsePositiveInt : Parser Int
parsePositiveInt =
    loop
        (\n ->
            bit
                |> andThen
                    (\b ->
                        case b of
                            O ->
                                succeed (Done n)

                            I ->
                                bits n
                                    |> map (\bs -> Loop (Bits.toIntUnsigned (I :: bs)))
                    )
        )
        1


encodeList : (a -> List Bit) -> List a -> List Bit
encodeList f list =
    encodeNonnegativeInt (List.length list) ++ List.concatMap f list


parseList : Parser a -> Parser (List a)
parseList item =
    parseNonnegativeInt
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


encodeString : String -> List Bit
encodeString string =
    [ encodeNonnegativeInt (Bytes.Encode.getStringWidth string)
    , Bits.fromBytes (Bytes.Encode.encode (Bytes.Encode.string string))
    ]
        |> List.concat


parseString : Parser String
parseString =
    parseNonnegativeInt
        |> andThen
            (\width ->
                bytes width
                    |> andThen
                        (\bs ->
                            case Bytes.Decode.decode (Bytes.Decode.string width) bs of
                                Just s ->
                                    succeed s

                                Nothing ->
                                    fail
                        )
            )
