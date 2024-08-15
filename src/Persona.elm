module Persona exposing (Config, Gendertrope(..), GendertropeRecord, Persona, default, encode, gendertropeToRecord, parser, view)

import Bit exposing (Bit)
import BitParser
import Element exposing (Attribute, Element, alignRight, centerX, centerY, el, fill, height, paragraph, px, rgb, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Icons
import Theme exposing (withHint)


type alias Persona =
    { name : String

    --
    , fitness : Int
    , grace : Int
    , ardor : Int
    , sanity : Int
    , prowess : Int
    , moxie : Int

    --
    , stamina : Int
    , satiation : Int
    , craving : Int
    , arousal : Int
    , sensitivity : Int

    --
    , euphoriaPoints : Int
    , ichorPoints : Int
    , numinousPoints : Int

    --
    , gendertrope : Gendertrope
    }


type Gendertrope
    = Butterfly
    | Flower
    | Vixen
    | Custom GendertropeRecord


standardGendertropes : List Gendertrope
standardGendertropes =
    [ Butterfly
    , Flower
    , Vixen
    ]


type alias GendertropeRecord =
    { name : String
    , description : String
    }


default : Persona
default =
    { name = "Cinderella Sheen"
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , stamina = 0
    , satiation = 0
    , craving = 0
    , arousal = 0
    , sensitivity = 0

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0

    --
    , gendertrope = Butterfly
    }


type alias Config msg =
    { update : Persona -> msg, flip : msg }


view :
    Config msg
    ->
        { a
            | flipped : Bool
            , persona : Persona
        }
    -> Element msg
view config { flipped, persona } =
    let
        style : String -> String -> Attribute msg
        style key value =
            Element.htmlAttribute <|
                Html.Attributes.style key value

        commonAttrs : Int -> List (Attribute msg)
        commonAttrs rotate =
            [ Border.width 1
            , width fill
            , height fill
            , Theme.padding
            , Background.color (rgb 1 1 1)
            , style "backface-visibility" "hidden"
            , style "transition" "all .5s ease-in-out"
            , style "position" "absolute"
            , style "transform"
                ("rotateY("
                    ++ String.fromInt rotate
                    ++ "deg)"
                )
            ]
    in
    Theme.row
        [ width <| px 600
        , height <| px 462
        , style "perspective" "1200px"
        ]
        [ Theme.column
            (commonAttrs
                (if flipped then
                    180

                 else
                    0
                )
            )
            [ nameRow config persona
            , Element.map config.update <|
                Theme.row [ width fill ]
                    [ abilitiesView persona
                    , statusView persona
                    ]
            , Element.map config.update <| progressionView persona
            ]
        , Theme.column
            (commonAttrs
                (if flipped then
                    360

                 else
                    180
                )
            )
            (Theme.row [ width fill ]
                [ text "Gendertrope"
                , Theme.button [ alignRight ]
                    { onPress = Just config.flip
                    , label = Icons.flip
                    }
                ]
                :: List.map
                    (Element.map (\newGendertrope -> config.update { persona | gendertrope = newGendertrope }))
                    (viewGendertrope persona.gendertrope)
            )
        ]


viewGendertrope : Gendertrope -> List (Element Gendertrope)
viewGendertrope gendertrope =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            gendertropeToRecord gendertrope

        radioRow : Element Gendertrope
        radioRow =
            Input.radioRow [ Theme.spacing ]
                { options =
                    (standardGendertropes ++ [ Custom gendertropeRecord ])
                        |> List.map
                            (\option ->
                                Input.optionWith
                                    option
                                    (\state ->
                                        let
                                            common : List (Attribute msg)
                                            common =
                                                [ Border.width 1
                                                , Theme.padding
                                                , Font.center
                                                , width fill
                                                ]
                                        in
                                        Theme.el
                                            (if state == Input.Selected then
                                                Background.color Theme.purple
                                                    :: Font.color (Element.rgb 1 1 1)
                                                    :: Border.color (Element.rgb 0 0 0)
                                                    :: common

                                             else
                                                Background.color Theme.gray
                                                    :: common
                                            )
                                        <|
                                            case option of
                                                Custom _ ->
                                                    text "Custom"

                                                _ ->
                                                    text (gendertropeToRecord option).name
                                    )
                            )
                , label = Input.labelHidden "Gendertrope kind"
                , onChange = identity
                , selected = Just gendertrope
                }
    in
    case gendertrope of
        Custom _ ->
            [ radioRow
            , Theme.input []
                { label = Input.labelHidden "Gendertrope"
                , onChange =
                    \newName ->
                        Custom
                            { gendertropeRecord
                                | name = newName
                            }
                , placeholder = Just <| Input.placeholder [] (text "Name")
                , text = gendertropeRecord.name
                }
            , Theme.multiline []
                { label = Input.labelHidden "Gendertrope - description"
                , onChange =
                    \newDescription ->
                        Custom
                            { gendertropeRecord
                                | description = newDescription
                            }
                , placeholder = Just <| Input.placeholder [] (text "Description")
                , text = gendertropeRecord.description
                , spellcheck = True
                }
            ]

        _ ->
            [ radioRow
            , paragraph [ Font.italic ]
                [ text gendertropeRecord.description
                ]
            ]


nameRow : Config msg -> Persona -> Element msg
nameRow config persona =
    Theme.row
        [ Element.paddingEach
            { top = 0
            , bottom = Theme.rhythm
            , left = 0
            , right = 0
            }
        , Border.widthEach
            { top = 0
            , right = 0
            , left = 0
            , bottom = 1
            }
        , width fill
        ]
        [ Theme.input [ width fill ]
            { label = Input.labelHidden "Name"
            , text = persona.name
            , onChange = \newValue -> config.update { persona | name = newValue }
            , placeholder =
                Just <|
                    Input.placeholder [] <|
                        text "Name"
            }
        , Theme.button []
            { onPress = Just config.flip
            , label = Icons.flip
            }
        ]


abilitiesView : Persona -> Element Persona
abilitiesView persona =
    let
        availablePoints : Int
        availablePoints =
            18
                - persona.fitness
                - persona.grace
                - persona.ardor
                - persona.sanity
                - persona.prowess
                - persona.moxie

        viewRow : ( Element Persona, Int, Int -> Persona ) -> Element Persona
        viewRow ( _, value, setter ) =
            Theme.row
                [ alignRight ]
                [ text (String.fromInt value)
                , Theme.button []
                    { label = Icons.minus
                    , onPress =
                        if value > 2 then
                            Just (setter (value - 1))

                        else
                            Nothing
                    }
                , Theme.button []
                    { label = Icons.plus
                    , onPress =
                        if availablePoints > 0 then
                            Just (setter (value + 1))

                        else
                            Nothing
                    }
                ]
    in
    Theme.column [ height fill, width fill ]
        [ text "Ability Scores"
        , Element.table [ Theme.spacing ]
            { data =
                [ ( withHint "Fitness" (text "FIT"), persona.fitness, \newValue -> { persona | fitness = newValue } )
                , ( withHint "Grace" (text "GRC"), persona.grace, \newValue -> { persona | grace = newValue } )
                , ( withHint "Ardor" (text "ARD"), persona.ardor, \newValue -> { persona | ardor = newValue } )
                , ( withHint "Sanity" (text "SAN"), persona.sanity, \newValue -> { persona | sanity = newValue } )
                , ( withHint "Prowess" (text "PRW"), persona.prowess, \newValue -> { persona | prowess = newValue } )
                , ( withHint "Moxie" (text "MOX"), persona.moxie, \newValue -> { persona | moxie = newValue } )
                ]
            , columns =
                [ { width = fill
                  , header = Element.none
                  , view = \( label, _, _ ) -> el [ centerY ] label
                  }
                , { width = shrink
                  , header = Element.none
                  , view = viewRow
                  }
                ]
            }
        ]


statusView : Persona -> Element Persona
statusView persona =
    let
        statusRow : String -> Int -> Int -> ( String, Int, Int )
        statusRow label value bonusToCap =
            ( label
            , value
            , 20 + 2 * bonusToCap
            )
    in
    Theme.column
        [ Border.widthEach
            { left = 1
            , top = 0
            , bottom = 0
            , right = 0
            }
        , Element.paddingEach
            { left = Theme.rhythm
            , top = 0
            , bottom = 0
            , right = 0
            }
        , height fill
        , width fill
        ]
        [ text "Status meters"
        , Element.table
            [ Theme.spacing
            ]
            { data =
                [ statusRow "Stamina" persona.stamina 0
                , statusRow "Satiation" persona.satiation persona.ardor
                , statusRow "Craving" persona.craving persona.sanity
                , statusRow "Arousal" persona.arousal persona.prowess
                , statusRow "Sensitivity" persona.sensitivity persona.moxie
                ]
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view = \( label, _, _ ) -> text label
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, value, _ ) -> text (String.fromInt value)
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \_ -> text "/"
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, _, maximum ) -> text (String.fromInt maximum)
                  }
                ]
            }
        ]


progressionView : Persona -> Element Persona
progressionView persona =
    Theme.column
        [ Border.widthEach
            { top = 1
            , left = 0
            , right = 0
            , bottom = 0
            }
        , Element.paddingEach
            { top = Theme.rhythm
            , left = 0
            , right = 0
            , bottom = 0
            }
        , width fill
        ]
        [ el [ centerX ] <| text "Progression Tally"
        , Theme.wrappedRow [ width fill ]
            [ viewPoints "EP" "Euphoria Points" persona.euphoriaPoints <| \newValue -> { persona | euphoriaPoints = newValue }
            , viewPoints "IP" "Ichor Points" persona.ichorPoints <| \newValue -> { persona | ichorPoints = newValue }
            , viewPoints "NP" "Numinous Points" persona.numinousPoints <| \newValue -> { persona | numinousPoints = newValue }
            ]
        ]


viewPoints : String -> String -> Int -> (Int -> Persona) -> Element Persona
viewPoints label fullName value setter =
    Theme.row [ width fill ]
        [ Theme.withHint fullName (text label)
        , Theme.wrappedRow [ width <| px (Theme.rhythm * 8) ]
            (List.repeat (value // 5) (tallyGroup 5)
                ++ [ tallyGroup (modBy 5 value) ]
            )
        , Theme.button [ alignRight ]
            { label = Icons.minus
            , onPress =
                if value > 0 then
                    Just (setter (value - 1))

                else
                    Nothing
            }
        , Theme.button [ alignRight ]
            { label = Icons.plus
            , onPress = Just (setter (value + 1))
            }
        ]


tallyGroup : Int -> Element msg
tallyGroup count =
    if count <= 0 then
        Element.none

    else
        row
            [ spacing (Theme.rhythm // 2)
            , Element.inFront
                (if count == 5 then
                    el
                        [ Border.widthEach
                            { bottom = 1
                            , top = 0
                            , right = 0
                            , left = 0
                            }
                        , centerY
                        , width fill
                        , Element.rotate (degrees -10)
                        ]
                        Element.none

                 else
                    Element.none
                )
            , Element.paddingXY (Theme.rhythm // 2) 0
            ]
            (List.repeat (min 4 count) tallyMark)


tallyMark : Element msg
tallyMark =
    el [ Border.width 1, height <| px 16 ] Element.none


gendertropeToRecord : Gendertrope -> GendertropeRecord
gendertropeToRecord gendertrope =
    case gendertrope of
        Butterfly ->
            { name = "The Butterfly"
            , description =
                "She is a creature of monstrous beauty and merciful power. Her amorous desires violate boundaries and overwhelm all resistance, rapacious and indomitable. But she is a nest-builder, a nurturer, one who cares for and cultivates that which her appetites have claimed as hers."
            }

        Flower ->
            { name = "The Flower"
            , description = "She is an object of tempting beauty and creative growth, decorative and useful in equal measure. Her body is offered freely to any who would take her, for her longing to be plucked and kept and tended by a worthy gardener runs deep. But she is fearless, and is not one who trades herself for safety. Only for joy."
            }

        Vixen ->
            { name = "The Vixen"
            , description = "She is one who embodies the predator, the explorer, and the protector all at once. Her whims and her primal hungers drive her to hunt and slake, to frolic and play, to guard and comfort, forever seeking excitement. But her mercurial heart is innocent and soft, gentle and always welcoming... just like her bosom."
            }

        Custom record ->
            record


parseGendertrope : BitParser.Parser Gendertrope
parseGendertrope =
    BitParser.parseNonnegativeInt
        |> BitParser.andThen
            (\i ->
                case i of
                    0 ->
                        BitParser.map2
                            (\name description -> Custom { name = name, description = description })
                            BitParser.parseString
                            BitParser.parseString

                    1 ->
                        BitParser.succeed Butterfly

                    2 ->
                        BitParser.succeed Flower

                    3 ->
                        BitParser.succeed Vixen

                    _ ->
                        BitParser.fail
            )


encodeGendertrope : Gendertrope -> List Bit
encodeGendertrope gendertrope =
    case gendertrope of
        Custom { name, description } ->
            [ BitParser.encodeNonnegativeInt 0
            , BitParser.encodeString name
            , BitParser.encodeString description
            ]
                |> List.concat

        Butterfly ->
            BitParser.encodeNonnegativeInt 1

        Flower ->
            BitParser.encodeNonnegativeInt 2

        Vixen ->
            BitParser.encodeNonnegativeInt 3


encode : Persona -> List Bit
encode persona =
    [ BitParser.encodeNonnegativeInt (persona.fitness - 2)
    , BitParser.encodeNonnegativeInt (persona.grace - 2)
    , BitParser.encodeNonnegativeInt (persona.ardor - 2)
    , BitParser.encodeNonnegativeInt (persona.sanity - 2)
    , BitParser.encodeNonnegativeInt (persona.prowess - 2)
    , BitParser.encodeNonnegativeInt (persona.moxie - 2)
    , BitParser.encodeNonnegativeInt persona.stamina
    , BitParser.encodeNonnegativeInt persona.satiation
    , BitParser.encodeNonnegativeInt persona.craving
    , BitParser.encodeNonnegativeInt persona.arousal
    , BitParser.encodeNonnegativeInt persona.sensitivity
    , BitParser.encodeNonnegativeInt persona.euphoriaPoints
    , BitParser.encodeNonnegativeInt persona.ichorPoints
    , BitParser.encodeNonnegativeInt persona.numinousPoints
    , encodeGendertrope persona.gendertrope
    ]
        |> List.concat


parser : String -> BitParser.Parser Persona
parser name =
    BitParser.succeed (Persona name)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap (BitParser.map (\n -> n + 2) BitParser.parseNonnegativeInt)
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap parseGendertrope
