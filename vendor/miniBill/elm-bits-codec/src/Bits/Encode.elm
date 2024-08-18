module Bits.Encode exposing (array, bool, int, list, nonNegativeInt, positiveInt, string)

import Array exposing (Array)
import Bit exposing (Bit)
import Bits
import Bytes.Encode
import Rope exposing (Rope)


int : Int -> Rope Bit
int n =
    nonNegativeInt
        (if n >= 0 then
            2 * n

         else
            -2 * n - 1
        )


nonNegativeInt : Int -> Rope Bit
nonNegativeInt n =
    positiveInt (n + 1)


positiveInt : Int -> Rope Bit
positiveInt i =
    if i < 1 then
        Rope.singleton Bit.O

    else
        positiveIntHelper i [ Bit.O ]


positiveIntHelper : Int -> List Bit -> Rope Bit
positiveIntHelper n acc =
    if n == 1 then
        Rope.fromList acc

    else
        let
            length : Int
            length =
                ceiling (logBase 2 (toFloat n + 1))
        in
        positiveIntHelper (length - 1) (Bits.fromIntUnsigned length n ++ acc)


array : (a -> Rope Bit) -> Array a -> Rope Bit
array item input =
    list item (Array.toList input)


list : (a -> Rope Bit) -> List a -> Rope Bit
list f input =
    List.foldl
        (\e acc -> acc |> Rope.prependTo (f e))
        (nonNegativeInt (List.length input))
        input


string : String -> Rope Bit
string input =
    nonNegativeInt (Bytes.Encode.getStringWidth input)
        |> Rope.prependTo
            (Rope.fromList
                (Bits.fromBytes (Bytes.Encode.encode (Bytes.Encode.string input)))
            )


bool : Bool -> Rope Bit
bool b =
    if b then
        Rope.singleton Bit.I

    else
        Rope.singleton Bit.O
