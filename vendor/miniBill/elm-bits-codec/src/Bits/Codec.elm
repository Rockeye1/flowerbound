module Bits.Codec exposing (Codec, CustomCodec, ObjectCodec, array, bool, buildCustom, buildObject, custom, dict, field, int, intWithMinimum, list, map, maybe, nonNegativeInt, object, positiveInt, string, succeed, tuple, variant0, variant1)

import Array exposing (Array)
import Bit exposing (Bit)
import Bits.Decode exposing (Decoder)
import Bits.Encode
import Dict exposing (Dict)
import Rope exposing (Rope)


type alias Codec e a =
    { encoder : a -> Rope Bit
    , decoder : Decoder e a
    }


bool : Codec e Bool
bool =
    { encoder = Bits.Encode.bool
    , decoder = Bits.Decode.bool
    }


int : Codec e Int
int =
    { encoder = Bits.Encode.int
    , decoder = Bits.Decode.int
    }


nonNegativeInt : Codec e Int
nonNegativeInt =
    { encoder = Bits.Encode.nonNegativeInt
    , decoder = Bits.Decode.nonNegativeInt
    }


positiveInt : Codec e Int
positiveInt =
    { encoder = Bits.Encode.positiveInt
    , decoder = Bits.Decode.positiveInt
    }


string : Codec e String
string =
    { encoder = Bits.Encode.string
    , decoder = Bits.Decode.string
    }


array : Codec e a -> Codec e (Array a)
array inner =
    composite Bits.Encode.array Bits.Decode.array inner


list : Codec e a -> Codec e (List a)
list inner =
    composite Bits.Encode.list Bits.Decode.list inner


maybe : Codec e a -> Codec e (Maybe a)
maybe inner =
    composite Bits.Encode.maybe Bits.Decode.maybe inner


composite :
    ((a -> Rope Bit) -> (b -> Rope Bit))
    -> (Decoder e a -> Decoder e b)
    -> Codec e a
    -> Codec e b
composite mapEncoder mapDecoder inner =
    { encoder = mapEncoder inner.encoder
    , decoder = mapDecoder inner.decoder
    }


dict : Codec e comparable -> Codec e v -> Codec e (Dict comparable v)
dict key value =
    tuple key value
        |> list
        |> map Dict.fromList Dict.toList


map : (a -> b) -> (b -> a) -> Codec e a -> Codec e b
map go back codec =
    { decoder = Bits.Decode.map go <| codec.decoder
    , encoder = \v -> back v |> codec.encoder
    }


tuple : Codec e a -> Codec e b -> Codec e ( a, b )
tuple first second =
    { encoder =
        \( f, s ) ->
            first.encoder f
                |> Rope.prependTo (second.encoder s)
    , decoder = Bits.Decode.map2 Tuple.pair first.decoder second.decoder
    }


succeed : a -> Codec e a
succeed x =
    { encoder = \_ -> Rope.empty
    , decoder = Bits.Decode.succeed x
    }


type ObjectCodec e a b
    = ObjectCodec
        { encoder : a -> Rope Bit
        , decoder : Decoder e b
        }


object : b -> ObjectCodec e a b
object ctor =
    ObjectCodec
        { encoder = \_ -> Rope.empty
        , decoder = Bits.Decode.succeed ctor
        }


field : (a -> f) -> Codec e f -> ObjectCodec e a (f -> b) -> ObjectCodec e a b
field getter fieldCodec (ObjectCodec objectCodec) =
    ObjectCodec
        { encoder =
            \v ->
                objectCodec.encoder v
                    |> Rope.prependTo (fieldCodec.encoder <| getter v)
        , decoder =
            objectCodec.decoder
                |> Bits.Decode.andMap fieldCodec.decoder
        }


buildObject : ObjectCodec e a a -> Codec e a
buildObject (ObjectCodec om) =
    om


type CustomCodec e match v
    = CustomCodec
        { match : match
        , decoder : Dict Int (Decoder e v)
        }


custom : match -> CustomCodec e match value
custom match =
    CustomCodec
        { match = match
        , decoder = Dict.empty
        }


{-| Define a variant with 0 parameters for a custom type.
-}
variant0 :
    v
    -> CustomCodec e (Rope Bit -> a) v
    -> CustomCodec e a v
variant0 ctor (CustomCodec previous) =
    let
        id : Int
        id =
            Dict.size previous.decoder
    in
    CustomCodec
        { match = previous.match (Bits.Encode.nonNegativeInt id)
        , decoder = Dict.insert id (Bits.Decode.succeed ctor) previous.decoder
        }


{-| Define a variant with 1 parameters for a custom type.
-}
variant1 :
    (a -> v)
    -> Codec e a
    -> CustomCodec e ((a -> Rope Bit) -> b) v
    -> CustomCodec e b v
variant1 ctor codec1 (CustomCodec previous) =
    let
        id : Int
        id =
            Dict.size previous.decoder
    in
    CustomCodec
        { match =
            previous.match
                (\v ->
                    Bits.Encode.nonNegativeInt id
                        |> Rope.prependTo (codec1.encoder v)
                )
        , decoder = Dict.insert id (Bits.Decode.map ctor codec1.decoder) previous.decoder
        }


buildCustom : CustomCodec e (a -> Rope Bit) a -> Codec e a
buildCustom (CustomCodec final) =
    { encoder = final.match
    , decoder =
        Bits.Decode.nonNegativeInt
            |> Bits.Decode.andThen
                (\id ->
                    case Dict.get id final.decoder of
                        Nothing ->
                            Bits.Decode.fail (Bits.Decode.VariantNotRecognized id)

                        Just dec ->
                            dec
                )
    }


intWithMinimum : Int -> Codec e Int
intWithMinimum min =
    map
        (\nn -> nn + min)
        (\v -> v - min)
        nonNegativeInt
