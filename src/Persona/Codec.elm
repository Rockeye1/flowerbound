module Persona.Codec exposing (fragmentToGendertropeRecord, fromUrl, maybeCompress, maybeDecompress, partialPersona, partialPersonaFromSlug, partialPersonaToSlug, personaParser, toString, toUrl)

import Base64
import Bit exposing (Bit)
import Bits
import Bits.Codec as Codec exposing (Codec)
import Bits.Decode
import Bytes exposing (Bytes)
import Dict
import Flate
import List.Extra
import Maybe.Extra
import Parser exposing ((|.), (|=), Parser)
import Parser.Workaround
import Persona
import Persona.Data as Data
import Rope
import Route exposing (Route)
import Types exposing (Appendage, Feature, Gendertrope(..), GendertropeRecord, Organ, OrganOrAppendage, OrganType(..), PartialGendertrope(..), PartialPersona, Persona)
import Url


fromUrl : String -> Result String Persona
fromUrl url =
    Url.fromString url
        |> Result.fromMaybe "Cannot parse URL"
        |> Result.andThen
            (\parsedUrl ->
                case Route.urlToRoute parsedUrl of
                    Just (Route.Persona__Name___Data__ { name, data }) ->
                        case data of
                            Nothing ->
                                Err "URL does not include enough data"

                            Just d ->
                                Ok
                                    { name =
                                        if name == " " then
                                            ""

                                        else
                                            name
                                    , data = d
                                    , fragment = parsedUrl.fragment
                                    }

                    _ ->
                        Err "Wrong URL"
            )
        |> Result.andThen
            (\{ name, data, fragment } ->
                Result.map3 Persona.fromPartial
                    (Url.percentDecode name
                        |> Result.fromMaybe "Could not decode name "
                        |> Result.map
                            (\decoded ->
                                if decoded == " " then
                                    ""

                                else
                                    decoded
                            )
                    )
                    (Result.map Just (partialPersonaFromSlug data))
                    (case fragment of
                        Nothing ->
                            Ok Nothing

                        Just f ->
                            case fragmentToGendertropeRecord f of
                                Nothing ->
                                    Err "Could not decode gendertrope data"

                                Just record ->
                                    Ok (Just record)
                    )
            )


toRoute : Persona -> Route
toRoute input =
    Route.Persona__Name___Data__
        { name =
            if String.isEmpty input.name then
                Url.percentEncode " "

            else
                Url.percentEncode input.name
        , data = Just (partialPersonaToSlug (Persona.toPartial input))
        }


toUrl : Persona -> String
toUrl input =
    let
        hash : String
        hash =
            gendertropeToHash input.gendertrope
    in
    if String.isEmpty hash then
        Route.toString (toRoute input)

    else
        Route.toString (toRoute input) ++ "#" ++ hash


partialPersonaFromSlug : String -> Result String PartialPersona
partialPersonaFromSlug slug =
    slugToBytes slug
        |> Result.andThen
            (\slugBytes ->
                slugBytes
                    |> Bits.Decode.run partialPersona.decoder
                    |> Result.mapError errorToString
            )


partialPersonaToSlug : PartialPersona -> String
partialPersonaToSlug input =
    input
        |> partialPersona.encoder
        |> Rope.toList
        |> Bits.toBytes
        |> bytesToSlug


bytesToSlug : Bytes -> String
bytesToSlug bytes =
    bytes
        |> maybeCompress
        |> Base64.fromBytes
        |> Maybe.withDefault ""
        |> String.replace "/" "_"
        |> String.replace "+" "-"


errorToString : Bits.Decode.Error String -> String
errorToString error =
    case error of
        Bits.Decode.Problem e ->
            e

        Bits.Decode.EndOfInput ->
            "End of input"

        Bits.Decode.StringParsingError ->
            "String parsing error"

        Bits.Decode.VariantNotRecognized n ->
            "Variant not recognized " ++ String.fromInt n


slugToBytes : String -> Result String (List Bit)
slugToBytes slug =
    slug
        |> String.replace "_" "/"
        |> String.replace "-" "+"
        |> Base64.toBytes
        |> Result.fromMaybe "Failed to base64 decode"
        |> Result.andThen
            (\bytes ->
                bytes
                    |> Bits.fromBytes
                    |> maybeDecompress
                    |> Result.fromMaybe "Could not decompress"
            )


gendertropeToHash : Gendertrope -> String
gendertropeToHash gendertrope =
    case gendertrope of
        Types.Custom record ->
            gendertropeRecord.encoder record
                |> Rope.toList
                |> Bits.toBytes
                |> bytesToSlug

        _ ->
            ""


maybeCompress : Bytes -> Bytes
maybeCompress input =
    let
        -- compressed : Bytes
        -- compressed =
        --     Flate.deflate input
        bits : List Bit
        bits =
            -- if Bytes.width compressed < Bytes.width input then
            --     Bit.I :: List.repeat 7 Bit.O ++ Bits.fromBytes compressed
            -- else
            Bit.O :: Bits.fromBytes input
    in
    Bits.toBytes bits


maybeDecompress : List Bit -> Maybe (List Bit)
maybeDecompress input =
    case input of
        [] ->
            Just input

        Bit.O :: tail ->
            Just tail

        Bit.I :: tail ->
            tail
                |> List.drop 7
                |> Bits.toBytes
                |> Flate.inflate
                |> Maybe.map Bits.fromBytes


fragmentToGendertropeRecord : String -> Maybe GendertropeRecord
fragmentToGendertropeRecord fragment =
    fragment
        |> slugToBytes
        |> Result.toMaybe
        |> Maybe.andThen
            (\bytes ->
                Bits.Decode.run gendertropeRecord.decoder bytes
                    |> Result.toMaybe
            )


personaParser : Parser Persona
personaParser =
    Parser.succeed Persona
        |= headerParser 1 (Parser.getChompedString (Parser.Workaround.chompUntilBefore "\n"))
        |= Parser.oneOf
            [ Parser.succeed Just
                |= ulParserThen "Hue" Parser.float
            , Parser.succeed Nothing
            ]
        |. headerParser 2 (Parser.keyword "Ability Scores")
        |= ulParserThen "Fitness" Parser.int
        |= ulParserThen "Grace" Parser.int
        |= ulParserThen "Ardor" Parser.int
        |= ulParserThen "Sanity" Parser.int
        |= ulParserThen "Prowess" Parser.int
        |= ulParserThen "Moxie" Parser.int
        |. headerParser 2 (Parser.keyword "Progression Tally")
        |= ulParserThen "Euphoria Points" integer
        |= ulParserThen "Ichor Points" integer
        |= ulParserThen "Numinous Points" integer
        |. headerParser 2 (Parser.keyword "Unlocked features")
        |= (Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = Parser.spaces
                , item =
                    Parser.succeed identity
                        |. Parser.symbol "-"
                        |. Parser.spaces
                        |. Parser.keyword "Level"
                        |. Parser.spaces
                        |= Parser.int
                , trailing = Parser.Optional
                }
                |> Parser.map (List.Extra.remove 1)
           )
        |. Parser.spaces
        |= gendertropeParser
        |. Parser.end
        |> Parser.map fixupPersona


integer : Parser Int
integer =
    Parser.succeed identity
        |= Parser.oneOf
            [ Parser.succeed negate
                |. Parser.symbol "-"
            , Parser.succeed identity
            ]
        |= Parser.int


fixupPersona : Persona -> Persona
fixupPersona persona =
    { persona
        | euphoriaPoints = persona.euphoriaPoints + Persona.usedEuphoriaPoints persona
        , ichorPoints = persona.ichorPoints + Persona.usedIchorPoints persona
        , numinousPoints = persona.numinousPoints + Persona.usedNuminousPoints persona
    }


toString : Persona -> String
toString persona =
    ([ block 1 persona.name []
        ++ (case persona.hue of
                Nothing ->
                    ""

                Just hue ->
                    "\n- Hue: " ++ String.fromFloat hue
           )
     , block 2
        "Ability Scores"
        (numericUl
            [ ( "Fitness", persona.fitness )
            , ( "Grace", persona.grace )
            , ( "Ardor", persona.ardor )
            , ( "Sanity", persona.sanity )
            , ( "Prowess", persona.prowess )
            , ( "Moxie", persona.moxie )
            ]
        )
     , block 2
        "Progression Tally"
        (numericUl
            [ ( "Euphoria Points", persona.euphoriaPoints - Persona.usedEuphoriaPoints persona )
            , ( "Ichor Points", persona.ichorPoints - Persona.usedIchorPoints persona )
            , ( "Numinous Points", persona.numinousPoints - Persona.usedNuminousPoints persona )
            ]
        )
     , block 2
        "Unlocked features"
        (persona.features
            |> (::) 1
            |> List.map
                (\l -> "- Level " ++ String.fromInt l)
        )
     ]
        ++ gendertropeToString persona.gendertrope
    )
        |> String.join "\n\n"


gendertropeToString : Gendertrope -> List String
gendertropeToString gendertrope =
    case gendertrope of
        Custom record ->
            gendertropeRecordToString record

        _ ->
            [ block 2 ("Gendertrope: " ++ (Data.gendertropeToRecord gendertrope).name) [] ]


gendertropeRecordToString : GendertropeRecord -> List String
gendertropeRecordToString gendertrope =
    [ block 2
        ("Gendertrope: " ++ gendertrope.name)
        [ gendertrope.description ]
    , block
        3
        "Organs"
        (gendertrope.organs
            |> List.map organToString
            |> ul
        )
    ]
        ++ List.map featureToString (Dict.toList gendertrope.features)
        ++ (case gendertrope.icon of
                Nothing ->
                    []

                Just { semitransparent, opaque } ->
                    [ block 3
                        "Icon"
                        (List.map (Tuple.pair "Semitransparent") semitransparent
                            ++ List.map (Tuple.pair "Opaque") opaque
                            |> ul
                        )
                    ]
           )


featureToString : ( Int, Feature ) -> String
featureToString ( level, value ) =
    block 3
        ("Level "
            ++ String.fromInt level
            ++ " Feature: "
            ++ avoidEmpty value.name
        )
        [ value.description ]


avoidEmpty : String -> String
avoidEmpty value =
    if String.isEmpty value then
        "-"

    else
        value


organToString : Organ -> ( String, String )
organToString value =
    let
        typeString : String
        typeString =
            Data.organTypeToString value.type_

        reference : Organ
        reference =
            organTypeToReference value.type_ value.name

        contourString : String
        contourString =
            if value.contour /= reference.contour then
                "\n  - Contour: "
                    ++ String.fromInt value.contour

            else
                ""

        erogenyString : String
        erogenyString =
            if value.erogeny /= reference.erogeny then
                "\n  - Erogeny: "
                    ++ String.fromInt value.erogeny

            else
                ""

        appendagesString : String
        appendagesString =
            if value.appendages /= reference.appendages then
                if List.isEmpty value.appendages then
                    "\n  - No appendages"

                else
                    value.appendages
                        |> List.map appendageString
                        |> String.join "\n"

            else
                ""

        appendageString : Appendage -> String
        appendageString app =
            "\n  - Appendage: "
                ++ app.name
                ++ String.join "\n  " (String.split "\n" (canString app value))
                ++ String.join "\n  " (String.split "\n" (isString app value))
    in
    ( typeString
    , value.name
        ++ contourString
        ++ erogenyString
        ++ canString value reference
        ++ isString value reference
        ++ appendagesString
    )


canString : OrganOrAppendage a -> OrganOrAppendage b -> String
canString value reference =
    if
        (value.canSquish /= reference.canSquish)
            || (value.canGrip /= reference.canGrip)
            || (value.canPenetrate /= reference.canPenetrate)
            || (value.canEnsheathe /= reference.canEnsheathe)
    then
        group "Can"
            [ ( "Squish", value.canSquish )
            , ( "Grip", value.canGrip )
            , ( "Penetrate", value.canPenetrate )
            , ( "Ensheathe", value.canEnsheathe )
            ]

    else
        ""


isString : OrganOrAppendage a -> OrganOrAppendage b -> String
isString value reference =
    if
        (value.isSquishable /= reference.isSquishable)
            || (value.isGrippable /= reference.isGrippable)
            || (value.isPenetrable /= reference.isPenetrable)
            || (value.isEnsheatheable /= reference.isEnsheatheable)
    then
        group "Is"
            [ ( "Squishable", value.isSquishable )
            , ( "Grippable", value.isGrippable )
            , ( "Penetrable", value.isPenetrable )
            , ( "Ensheatheable", value.isEnsheatheable )
            ]

    else
        ""


group : String -> List ( String, Bool ) -> String
group label items =
    ("\n  - " ++ label ++ ":")
        :: List.filterMap
            (\( item, enable ) ->
                if enable then
                    Just item

                else
                    Nothing
            )
            items
        |> String.join "\n    - "


organTypeToReference : OrganType -> String -> Organ
organTypeToReference type_ name =
    case type_ of
        Mouth ->
            Data.mouth name

        Hands ->
            Data.hands name

        Breasts ->
            Data.breasts name "Valley"

        Hips ->
            Data.hips name

        Yonic ->
            Data.yonic name

        Phallic ->
            Data.phallic name

        Legs ->
            Data.legs name "Tight"

        Prehensile ->
            Data.prehensile name

        Other ->
            Data.other name


block : Int -> String -> List String -> String
block level name children =
    (String.repeat level "#" ++ " " ++ avoidEmpty name)
        :: children
        |> String.join "\n"


numericUl : List ( String, Int ) -> List String
numericUl children =
    let
        li : ( String, Int ) -> String
        li ( key, value ) =
            "- " ++ key ++ ": " ++ String.fromInt value
    in
    List.map li children


ul : List ( String, String ) -> List String
ul children =
    let
        li : ( String, String ) -> String
        li ( key, value ) =
            "- " ++ key ++ ": " ++ value
    in
    List.map li children


gendertropeParser : Parser Gendertrope
gendertropeParser =
    let
        nameParser : Parser String
        nameParser =
            Parser.succeed identity
                |. Parser.symbol "##"
                |. Parser.spaces
                |. Parser.keyword "Gendertrope:"
                |. Parser.spaces
                |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "\n")
                |. Parser.spaces
    in
    nameParser
        |> Parser.andThen
            (\name ->
                case Data.gendertropeFromName name of
                    Just gendertrope ->
                        Parser.oneOf
                            [ Parser.succeed gendertrope
                                |. Parser.end
                            , Parser.map Custom (gendertropeRecordParser name)
                            ]

                    Nothing ->
                        Parser.map Custom (gendertropeRecordParser name)
            )


gendertropeRecordParser : String -> Parser GendertropeRecord
gendertropeRecordParser name =
    Parser.succeed (GendertropeRecord name)
        |= Parser.map String.trim (Parser.getChompedString (Parser.Workaround.chompUntilBefore "###"))
        |. headerParser 3 (Parser.keyword "Organs")
        |= Parser.sequence
            { start = ""
            , end = ""
            , separator = ""
            , spaces = Parser.spaces
            , item = organParser
            , trailing = Parser.Optional
            }
        |= (Parser.succeed Dict.fromList
                |= Parser.sequence
                    { start = ""
                    , end = ""
                    , separator = ""
                    , spaces = Parser.spaces
                    , item =
                        Parser.succeed Tuple.pair
                            |= headerParser 3
                                (Parser.succeed identity
                                    |. Parser.keyword "Level"
                                    |. Parser.spaces
                                    |= Parser.int
                                    |. Parser.spaces
                                    |. Parser.keyword "Feature"
                                    |. Parser.spaces
                                    |. Parser.symbol ":"
                                )
                            |= featureParser
                    , trailing = Parser.Optional
                    }
           )
        |= Parser.oneOf
            [ Parser.succeed
                (\pieces ->
                    let
                        ( opaque, semitransparent ) =
                            List.partition .opaque pieces
                    in
                    { semitransparent = List.map .path semitransparent
                    , opaque = List.map .path opaque
                    }
                        |> Just
                )
                |. headerParser 3 (Parser.keyword "Icon")
                |= Parser.sequence
                    { start = ""
                    , end = ""
                    , separator = ""
                    , spaces = Parser.spaces
                    , item =
                        Parser.succeed (\opaque path -> { opaque = opaque, path = path })
                            |= Parser.oneOf
                                [ ulParser "Semitransparent" False
                                , ulParser "Opaque" True
                                ]
                            |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "\n")
                    , trailing = Parser.Optional
                    }
            , Parser.succeed Nothing
            ]


organParser : Parser Organ
organParser =
    let
        groupParser : String -> List String -> Parser (Maybe (List String))
        groupParser label options =
            Parser.oneOf
                [ Parser.succeed Just
                    |. ulParser label ()
                    |= Parser.sequence
                        { start = ""
                        , end = ""
                        , separator = ""
                        , spaces = Parser.spaces
                        , item =
                            Parser.succeed identity
                                |. Parser.backtrackable (Parser.symbol "-")
                                |. Parser.backtrackable Parser.spaces
                                |= Parser.getChompedString
                                    (options
                                        |> List.map Parser.keyword
                                        |> Parser.oneOf
                                    )
                                |. Parser.commit ()
                        , trailing = Parser.Optional
                        }
                , Parser.succeed Nothing
                ]

        appendageParser : Parser (OrganOrAppendage a -> Appendage)
        appendageParser =
            Parser.succeed
                (\name maybeCan maybeIs maybeCan2 partialOrgan ->
                    { name = name
                    , canSquish = partialOrgan.canSquish
                    , canGrip = partialOrgan.canGrip
                    , canPenetrate = partialOrgan.canPenetrate
                    , canEnsheathe = partialOrgan.canEnsheathe
                    , isSquishable = partialOrgan.isSquishable
                    , isGrippable = partialOrgan.isGrippable
                    , isPenetrable = partialOrgan.isPenetrable
                    , isEnsheatheable = partialOrgan.isEnsheatheable
                    }
                        |> withCan (maybeCan |> Maybe.Extra.orElse maybeCan2)
                        |> withIs maybeIs
                )
                |= ulParserThen "Appendage"
                    (Parser.Workaround.chompUntilBefore "\n"
                        |> Parser.getChompedString
                    )
                |= groupParser "Can"
                    [ "Squish"
                    , "Grip"
                    , "Penetrate"
                    , "Ensheathe"
                    ]
                |= groupParser "Is"
                    [ "Squishable"
                    , "Grippable"
                    , "Penetrable"
                    , "Ensheatheable"
                    ]
                |= groupParser "Can"
                    [ "Squish"
                    , "Grip"
                    , "Penetrate"
                    , "Ensheathe"
                    ]
    in
    Parser.succeed
        (\type_ name contour erogeny maybeCan maybeIs maybeCan2 appendages ->
            let
                withNumbersAndAppendages : Organ -> Organ
                withNumbersAndAppendages partial =
                    { partial
                        | contour = contour |> Maybe.withDefault partial.contour
                        , erogeny = erogeny |> Maybe.withDefault partial.erogeny
                    }

                withAppendages : Organ -> Organ
                withAppendages partial =
                    { partial
                        | appendages =
                            case appendages partial of
                                Just [] ->
                                    partial.appendages

                                Just list ->
                                    list

                                Nothing ->
                                    []
                    }
            in
            organTypeToReference type_ name
                |> withNumbersAndAppendages
                |> withCan (maybeCan |> Maybe.Extra.orElse maybeCan2)
                |> withIs maybeIs
                |> withAppendages
        )
        |= Parser.oneOf
            [ ulParser "Custom" Other
            , ulParser "Mouth" Mouth
            , ulParser "Hands" Hands
            , ulParser "Breasts" Breasts
            , ulParser "Hips" Hips
            , ulParser "Legs" Legs
            , ulParser "Phallic" Phallic
            , ulParser "Yonic" Yonic
            , ulParser "Prehensile" Prehensile
            ]
        |= Parser.getChompedString (Parser.Workaround.chompUntilBefore "\n")
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.succeed Just
                |= ulParserThen "Contour" Parser.int
            , Parser.succeed Nothing
            ]
        |= Parser.oneOf
            [ Parser.succeed Just
                |= ulParserThen "Erogeny" Parser.int
            , Parser.succeed Nothing
            ]
        |= groupParser "Can"
            [ "Squish"
            , "Grip"
            , "Penetrate"
            , "Ensheathe"
            ]
        |= groupParser "Is"
            [ "Squishable"
            , "Grippable"
            , "Penetrable"
            , "Ensheatheable"
            ]
        |= groupParser "Can"
            [ "Squish"
            , "Grip"
            , "Penetrate"
            , "Ensheathe"
            ]
        |= Parser.oneOf
            [ Parser.succeed (\_ -> Nothing)
                |. Parser.backtrackable (Parser.symbol "-")
                |. Parser.backtrackable Parser.spaces
                |. Parser.keyword "No"
                |. Parser.spaces
                |. Parser.keyword "appendages"
                |. Parser.commit ()
                |. Parser.spaces
            , Parser.sequence
                { start = ""
                , end = ""
                , separator = ""
                , spaces = Parser.spaces
                , item = appendageParser
                , trailing = Parser.Optional
                }
                |> Parser.map
                    (\list partial ->
                        List.map
                            (\a -> a partial)
                            list
                            |> Just
                    )
            ]
        |. Parser.spaces


withCan : Maybe (List String) -> OrganOrAppendage a -> OrganOrAppendage a
withCan maybeCan partial =
    case maybeCan of
        Just can ->
            { partial
                | canSquish = List.member "Squish" can
                , canGrip = List.member "Grip" can
                , canPenetrate = List.member "Penetrate" can
                , canEnsheathe = List.member "Ensheathe" can
            }

        Nothing ->
            partial


withIs : Maybe (List String) -> OrganOrAppendage a -> OrganOrAppendage a
withIs maybeIs partial =
    case maybeIs of
        Just is ->
            { partial
                | isSquishable = List.member "Squishable" is
                , isGrippable = List.member "Grippable" is
                , isPenetrable = List.member "Penetrable" is
                , isEnsheatheable = List.member "Ensheatheable" is
            }

        Nothing ->
            partial


featureParser : Parser Feature
featureParser =
    Parser.succeed Feature
        |= Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "\n")
        |. Parser.spaces
        |= Parser.map String.trim (Parser.getChompedString (Parser.Workaround.chompUntilEndOrBefore "###"))


headerParser : Int -> Parser a -> Parser a
headerParser level inner =
    let
        hashes : String
        hashes =
            String.repeat level "#"
    in
    Parser.succeed identity
        |. Parser.backtrackable
            (Parser.symbol hashes
                |. Parser.spaces
            )
        |= inner
        |. Parser.commit ()
        |. Parser.spaces


ulParser : String -> a -> Parser a
ulParser name result =
    Parser.succeed result
        |. Parser.backtrackable (Parser.symbol "-")
        |. Parser.backtrackable Parser.spaces
        |. Parser.keyword name
        |. Parser.commit ()
        |. Parser.spaces
        |. Parser.symbol ":"
        |. Parser.spaces


ulParserThen : String -> Parser a -> Parser a
ulParserThen name inner =
    Parser.succeed identity
        |. ulParser name ()
        |= inner
        |. Parser.spaces


partialPersona : Codec e PartialPersona
partialPersona =
    Codec.object PartialPersona
        |> Codec.field .hue (Codec.maybe Codec.float)
        |> Codec.field .fitness (Codec.intWithMinimum 2)
        |> Codec.field .grace (Codec.intWithMinimum 2)
        |> Codec.field .ardor (Codec.intWithMinimum 2)
        |> Codec.field .sanity (Codec.intWithMinimum 2)
        |> Codec.field .prowess (Codec.intWithMinimum 2)
        |> Codec.field .moxie (Codec.intWithMinimum 2)
        |> Codec.field .euphoriaPoints Codec.nonNegativeInt
        |> Codec.field .ichorPoints Codec.nonNegativeInt
        |> Codec.field .numinousPoints Codec.nonNegativeInt
        |> Codec.field (.features >> List.Extra.remove 1) (Codec.list (Codec.intWithMinimum 2))
        |> Codec.field .gendertrope partialGendertrope
        |> Codec.buildObject


feature : Codec e Feature
feature =
    Codec.object Feature
        |> Codec.field .name Codec.string
        |> Codec.field .description Codec.string
        |> Codec.buildObject


partialGendertrope : Codec e PartialGendertrope
partialGendertrope =
    Codec.custom
        (\fCustom fButterfly fFlower fVixen fBuck fFiend fDoll fSeedStalker fHousepet fJunglePrince fFirecracker fDemonQueen fEldritch value ->
            case value of
                PartialCustom name ->
                    fCustom name

                PartialButterfly ->
                    fButterfly

                PartialFlower ->
                    fFlower

                PartialVixen ->
                    fVixen

                PartialBuck ->
                    fBuck

                PartialFiend ->
                    fFiend

                PartialDoll ->
                    fDoll

                PartialSeedStalker ->
                    fSeedStalker

                PartialHousepet ->
                    fHousepet

                PartialJunglePrince ->
                    fJunglePrince

                PartialFirecracker ->
                    fFirecracker

                PartialDemonQueen ->
                    fDemonQueen

                PartialEldritch ->
                    fEldritch
        )
        |> Codec.variant1 PartialCustom Codec.string
        |> Codec.variant0 PartialButterfly
        |> Codec.variant0 PartialFlower
        |> Codec.variant0 PartialVixen
        |> Codec.variant0 PartialBuck
        |> Codec.variant0 PartialFiend
        |> Codec.variant0 PartialDoll
        |> Codec.variant0 PartialSeedStalker
        |> Codec.variant0 PartialHousepet
        |> Codec.variant0 PartialJunglePrince
        |> Codec.variant0 PartialFirecracker
        |> Codec.variant0 PartialDemonQueen
        |> Codec.variant0 PartialEldritch
        |> Codec.buildCustom


gendertropeRecord : Codec e GendertropeRecord
gendertropeRecord =
    Codec.object GendertropeRecord
        |> Codec.field .name Codec.string
        |> Codec.field .description Codec.string
        |> Codec.field .organs (Codec.list organ)
        |> Codec.field .features (Codec.dict Codec.positiveInt feature)
        |> Codec.field .icon (Codec.maybe iconCodec)
        |> Codec.buildObject


iconCodec : Codec e { opaque : List String, semitransparent : List String }
iconCodec =
    Codec.object
        (\opaque semitransparent ->
            { opaque = opaque
            , semitransparent = semitransparent
            }
        )
        |> Codec.field .opaque (Codec.list Codec.string)
        |> Codec.field .semitransparent (Codec.list Codec.string)
        |> Codec.buildObject


organ : Codec e Organ
organ =
    Codec.object Organ
        |> Codec.field .name Codec.string
        |> Codec.field .type_ organTypeCodec
        |> Codec.field .contour Codec.nonNegativeInt
        |> Codec.field .erogeny Codec.nonNegativeInt
        |> Codec.field .appendages (Codec.list appendage)
        |> Codec.field .canSquish Codec.bool
        |> Codec.field .canGrip Codec.bool
        |> Codec.field .canPenetrate Codec.bool
        |> Codec.field .canEnsheathe Codec.bool
        |> Codec.field .isSquishable Codec.bool
        |> Codec.field .isGrippable Codec.bool
        |> Codec.field .isPenetrable Codec.bool
        |> Codec.field .isEnsheatheable Codec.bool
        |> Codec.buildObject


appendage : Codec e Appendage
appendage =
    Codec.object Appendage
        |> Codec.field .name Codec.string
        |> Codec.field .canSquish Codec.bool
        |> Codec.field .canGrip Codec.bool
        |> Codec.field .canPenetrate Codec.bool
        |> Codec.field .canEnsheathe Codec.bool
        |> Codec.field .isSquishable Codec.bool
        |> Codec.field .isGrippable Codec.bool
        |> Codec.field .isPenetrable Codec.bool
        |> Codec.field .isEnsheatheable Codec.bool
        |> Codec.buildObject


organTypeCodec : Codec e OrganType
organTypeCodec =
    Codec.custom
        (\mouth hands breasts hips yonic phallic legs prehensile other value ->
            case value of
                Mouth ->
                    mouth

                Hands ->
                    hands

                Breasts ->
                    breasts

                Hips ->
                    hips

                Yonic ->
                    yonic

                Phallic ->
                    phallic

                Legs ->
                    legs

                Prehensile ->
                    prehensile

                Other ->
                    other
        )
        |> Codec.variant0 Mouth
        |> Codec.variant0 Hands
        |> Codec.variant0 Breasts
        |> Codec.variant0 Hips
        |> Codec.variant0 Yonic
        |> Codec.variant0 Phallic
        |> Codec.variant0 Legs
        |> Codec.variant0 Prehensile
        |> Codec.variant0 Other
        |> Codec.buildCustom
