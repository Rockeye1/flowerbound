module Persona exposing (Config, Feature, Gendertrope(..), GendertropeRecord, Organ, Persona, default, encode, gendertropeToRecord, levelBonus, parser, view)

import Bit exposing (Bit)
import BitParser
import Dict exposing (Dict)
import Element exposing (Attribute, Element, alignBottom, alignRight, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Icons
import List.Extra
import Markdown.Block
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer
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
    , features : List Int
    }


type Gendertrope
    = Butterfly
    | Flower
    | Vixen
    | Buck
    | Fiend
    | Doll
    | Custom GendertropeRecord


standardGendertropes : List Gendertrope
standardGendertropes =
    [ Butterfly
    , Flower
    , Vixen
    , Buck
    , Fiend
    , Doll
    ]


type alias GendertropeRecord =
    { name : String
    , description : String
    , features : Dict Int Feature
    , organs : List Organ
    }


type alias Feature =
    { name : String
    , description : String
    }


type alias Organ =
    { name : String
    , contour : Int
    , erogeny : Int
    , canSquish : Bool
    , canGrip : Bool
    , canPenetrate : Bool
    , canEnsheathe : Bool
    , isSquishable : Bool
    , isGrippable : Bool
    , isPenetrable : Bool
    , isEnsheatheable : Bool
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
    , features = []
    }


type alias Config msg =
    { update : Persona -> msg, flip : msg }


levelBonus : Persona -> Int
levelBonus persona =
    let
        highestSum : Int
        highestSum =
            [ persona.fitness
            , persona.grace
            , persona.ardor
            , persona.sanity
            , persona.prowess
            , persona.moxie
            ]
                |> List.sort
                |> List.drop 4
                |> List.sum

        fromAbilityScores : Int
        fromAbilityScores =
            highestSum // 20

        fromFeatures : Int
        fromFeatures =
            -- The +1 is for the implicit level 1 feature
            (List.length persona.features + 1) // 2
    in
    max 1
        (fromAbilityScores + fromFeatures)


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
        fullWidth : number
        fullWidth =
            800

        style : String -> String -> Attribute msg
        style key value =
            Element.htmlAttribute <|
                Html.Attributes.style key value

        commonAttrs : Int -> List (Attribute msg)
        commonAttrs rotate =
            [ Border.width 1
            , width <| px fullWidth
            , height fill
            , Theme.padding
            , Background.color Theme.white
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
        [ width <| px fullWidth
        , height <| px 600
        , style "perspective" "2000px"
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
                    (Element.map
                        (\msg ->
                            case msg of
                                Ok newGendertrope ->
                                    config.update { persona | gendertrope = newGendertrope }

                                Err newFeatures ->
                                    config.update { persona | features = newFeatures }
                        )
                    )
                    (viewGendertrope persona)
            )
        ]


viewGendertrope : Persona -> List (Element (Result (List Int) Gendertrope))
viewGendertrope ({ gendertrope } as persona) =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            gendertropeToRecord gendertrope

        radioRow : Element (Result e Gendertrope)
        radioRow =
            let
                common : List (Attribute msg)
                common =
                    [ Border.width 1
                    , Theme.padding
                    , Font.center
                    , width fill
                    ]
            in
            (standardGendertropes ++ [ Custom gendertropeRecord ])
                |> List.map
                    (\option ->
                        Input.button
                            (if option == gendertrope then
                                Background.color Theme.purple
                                    :: Font.color Theme.white
                                    :: Border.color Theme.black
                                    :: common

                             else
                                Background.color Theme.gray
                                    :: Border.color Theme.purple
                                    :: common
                            )
                            { label =
                                case option of
                                    Custom _ ->
                                        text "Custom"

                                    _ ->
                                        text (gendertropeToRecord option).name
                            , onPress = Just (Ok option)
                            }
                    )
                |> Theme.wrappedRow [ width fill ]
    in
    case gendertrope of
        Custom _ ->
            [ radioRow
            , Theme.input []
                { label = Input.labelHidden "Gendertrope"
                , onChange =
                    \newName ->
                        { gendertropeRecord
                            | name = newName
                        }
                            |> Custom
                            |> Ok
                , placeholder = Just <| Input.placeholder [] (text "Name")
                , text = gendertropeRecord.name
                }
            , Theme.multiline []
                { label = Input.labelHidden "Gendertrope - description"
                , onChange =
                    \newDescription ->
                        { gendertropeRecord
                            | description = newDescription
                        }
                            |> Custom
                            |> Ok
                , placeholder = Just <| Input.placeholder [] (text "Description")
                , text = gendertropeRecord.description
                , spellcheck = True
                }
            , Element.map
                (\newOrgans ->
                    { gendertropeRecord
                        | organs = newOrgans
                    }
                        |> Custom
                        |> Ok
                )
                (viewOrgans gendertropeRecord)
            , viewFeatures persona gendertropeRecord
            ]

        _ ->
            [ radioRow
            , paragraph [ Font.italic ]
                [ text gendertropeRecord.description
                ]
            , viewStandardOrgans gendertropeRecord
            , Element.map Err (viewStandardFeatures persona gendertropeRecord)
            ]


viewStandardOrgans : GendertropeRecord -> Element msg
viewStandardOrgans gendertropeRecord =
    let
        wrap : Int -> Element msg -> Element msg
        wrap index child =
            el
                [ width fill
                , height fill
                , padding (Theme.rhythm // 2)
                , Background.color
                    (if modBy 2 index == 0 then
                        Theme.lightGray

                     else
                        Theme.white
                    )
                ]
                child

        intColumn :
            String
            -> (Organ -> Int)
            -> Element.IndexedColumn Organ msg
        intColumn label prop =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    wrap index
                        -- (el
                        --     [ Font.color Theme.purple
                        --     , Font.size 24
                        --     , centerX
                        --     ]
                        --     (text (intToDots (prop organ)))
                        -- )
                        (el
                            [ centerX
                            ]
                            (text (String.fromInt (prop organ)))
                        )
            }

        boolColumn :
            String
            -> (Organ -> Bool)
            -> Element msg
            -> Element.IndexedColumn Organ msg
        boolColumn label prop icon =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    if prop organ then
                        wrap index (el [ centerX ] icon)

                    else
                        wrap index Element.none
            }

        spacer : Element.IndexedColumn organ msg
        spacer =
            { width = fill
            , header = Element.none
            , view = \_ _ -> Element.none
            }
    in
    Element.indexedTable [ width fill ]
        { data = gendertropeRecord.organs
        , columns =
            [ spacer
            , { width = shrink
              , header = Element.none
              , view = \index { name } -> wrap index (text name)
              }
            , intColumn "Cont" .contour
            , intColumn "Erog" .erogeny
            , boolColumn "CS" .canSquish (text "\u{1FAF8}")
            , boolColumn "CG" .canGrip (text "🤏")
            , boolColumn "CP" .canPenetrate (text "☝️")
            , boolColumn "CE" .canEnsheathe (text "👌")
            , boolColumn "IS" .isSquishable (text "❤️")
            , boolColumn "IG" .isGrippable (text "🕹️")
            , boolColumn "IP" .isPenetrable (text "🕳️")
            , boolColumn "IE" .isEnsheatheable (text "🍆")
            , spacer
            ]
        }


viewOrgans : GendertropeRecord -> Element msg
viewOrgans gendertropeRecord =
    let
        wrap : Int -> Element msg -> Element msg
        wrap index child =
            el
                [ width fill
                , height fill
                , padding (Theme.rhythm // 2)
                , Background.color
                    (if modBy 2 index == 0 then
                        Theme.lightGray

                     else
                        Theme.white
                    )
                ]
                child

        intColumn :
            String
            -> (Organ -> Int)
            -> Element.IndexedColumn Organ msg
        intColumn label prop =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    wrap index
                        (el
                            [ Font.color Theme.purple
                            , Font.size 24
                            , centerX
                            ]
                            (text (intToDots (prop organ)))
                        )
            }

        boolColumn :
            String
            -> (Organ -> Bool)
            -> Element msg
            -> Element.IndexedColumn Organ msg
        boolColumn label prop icon =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    if prop organ then
                        wrap index (el [ centerX ] icon)

                    else
                        wrap index Element.none
            }

        spacer : Element.IndexedColumn organ msg
        spacer =
            { width = fill
            , header = Element.none
            , view = \_ _ -> Element.none
            }
    in
    Element.indexedTable [ width fill ]
        { data = gendertropeRecord.organs
        , columns =
            [ spacer
            , { width = shrink
              , header = Element.none
              , view = \index { name } -> wrap index (text name)
              }
            , intColumn "Cont" .contour
            , intColumn "Erog" .erogeny
            , boolColumn "CS" .canSquish (text "\u{1FAF8}")
            , boolColumn "CG" .canGrip (text "🤏")
            , boolColumn "CP" .canPenetrate (text "☝️")
            , boolColumn "CE" .canEnsheathe (text "👌")
            , boolColumn "IS" .isSquishable (text "❤️")
            , boolColumn "IG" .isGrippable (text "🕹️")
            , boolColumn "IP" .isPenetrable (text "🕳️")
            , boolColumn "IE" .isEnsheatheable (text "🍆")
            , spacer
            ]
        }


intToDots : Int -> String
intToDots i =
    case i of
        0 ->
            "⠀"

        1 ->
            "⠄"

        2 ->
            "⠤"

        3 ->
            "⠦"

        4 ->
            "⠶"

        5 ->
            "⠷"

        -- 6 ->
        --     "⠿"
        -- 7 ->
        --     "⣷"
        -- 8 ->
        --     "⣿"
        _ ->
            "⠷" ++ intToDots (i - 5)


viewStandardFeatures : Persona -> GendertropeRecord -> Element (List Int)
viewStandardFeatures ({ features } as persona) gendertropeRecord =
    let
        viewStandardFeature : ( Int, Feature ) -> Element (List Int)
        viewStandardFeature ( level, feature ) =
            let
                selected : Bool
                selected =
                    level == 1 || List.member level features

                canSelect : Bool
                canSelect =
                    selected || (persona.euphoriaPoints - usedEuphoriaPoints persona) >= 10 + level
            in
            Theme.button
                [ Font.alignLeft
                , padding 0
                , Border.width 0
                , width fill
                ]
                { onPress =
                    if level == 1 || not canSelect then
                        Nothing

                    else if selected then
                        Just (List.Extra.remove level features)

                    else
                        Just (level :: features)
                , label =
                    Theme.column
                        [ Border.width 1
                        , Theme.padding
                        , width fill
                        , Background.color
                            (if selected then
                                Theme.purple

                             else if canSelect then
                                Theme.white

                             else
                                Theme.gray
                            )
                        , Font.color
                            (if selected then
                                Theme.white

                             else
                                Theme.black
                            )
                        ]
                        (paragraph [ Font.underline ]
                            [ text ("Level " ++ String.fromInt level ++ " Feature: ")
                            , el [ Font.bold ] (text feature.name)
                            ]
                            :: viewMarkdown feature.description
                        )
                }
    in
    gendertropeRecord.features
        |> Dict.toList
        |> List.map viewStandardFeature
        |> Theme.column [ width fill ]


viewFeatures : Persona -> GendertropeRecord -> Element (Result (List Int) Gendertrope)
viewFeatures ({ features } as persona) gendertropeRecord =
    let
        viewFeature : ( Int, Feature ) -> Element (Result (List Int) Gendertrope)
        viewFeature ( level, feature ) =
            let
                selected : Bool
                selected =
                    level == 1 || List.member level features

                canSelect : Bool
                canSelect =
                    selected || (persona.euphoriaPoints - usedEuphoriaPoints persona) >= 10 + level
            in
            Theme.button
                [ Font.alignLeft
                , padding 0
                , Border.width 0
                , width fill
                ]
                { onPress =
                    if level == 1 || not canSelect then
                        Nothing

                    else if selected then
                        Just (Err (List.Extra.remove level features))

                    else
                        Just (Err (level :: features))
                , label =
                    Theme.column
                        [ Border.width 1
                        , Theme.padding
                        , width fill
                        , Background.color
                            (if selected then
                                Theme.purple

                             else if canSelect then
                                Theme.white

                             else
                                Theme.gray
                            )
                        , Font.color
                            (if selected then
                                Theme.white

                             else
                                Theme.black
                            )
                        ]
                        (paragraph [ Font.underline ]
                            [ text ("Level " ++ String.fromInt level ++ " Feature: ")
                            , el [ Font.bold ] (text feature.name)
                            ]
                            :: viewMarkdown feature.description
                        )
                }
    in
    gendertropeRecord.features
        |> Dict.toList
        |> List.map viewFeature
        |> Theme.column [ width fill ]


viewMarkdown : String -> List (Element msg)
viewMarkdown markdown =
    case Markdown.Parser.parse markdown of
        Ok blocks ->
            viewMarkdownBlocks blocks

        Err _ ->
            [ text "Could not parse Markdown" ]


viewMarkdownBlocks : List Markdown.Block.Block -> List (Element msg)
viewMarkdownBlocks blocks =
    case Markdown.Renderer.render markdownRenderer blocks of
        Ok elem ->
            elem

        Err e ->
            [ text e ]


markdownRenderer : Markdown.Renderer.Renderer (Element msg)
markdownRenderer =
    { heading = \_ -> text "TODO: heading"
    , paragraph = paragraph [ Theme.spacing ]
    , blockQuote =
        column
            [ Theme.padding
            , Theme.spacing
            , Border.width 1
            ]
    , html = Markdown.Html.oneOf []
    , text = text
    , codeSpan = \_ -> text "TODO: codeSpan"
    , strong = row [ Font.bold ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , hardLineBreak = Html.br [] [] |> Element.html
    , link =
        \{ title, destination } body ->
            Element.newTabLink
                [ maybeTitle title
                , Element.htmlAttribute (Html.Attributes.style "display" "inline-flex")
                ]
                { url = destination
                , label =
                    Element.paragraph
                        [ Font.color (Element.rgb255 0 0 255)
                        ]
                        body
                }
    , image =
        \image ->
            Element.image
                [ Element.width Element.fill
                , maybeTitle image.title
                ]
                { src = image.src, description = image.alt }
    , unorderedList = \_ -> text "TODO: unorderedList"
    , orderedList = \_ _ -> text "TODO: orderedList"
    , codeBlock = \_ -> text "TODO: codeBlock"
    , thematicBreak = Element.none
    , table = \_ -> text "TODO: table"
    , tableHeader = \_ -> text "TODO: tableHeader"
    , tableBody = \_ -> text "TODO: tableBody"
    , tableRow = \_ -> text "TODO: tableRow"
    , tableCell = \_ _ -> text "TODO: tableCell"
    , tableHeaderCell = \_ _ -> text "TODO: tableHeaderCell"
    }


maybeTitle : Maybe String -> Attribute msg
maybeTitle title =
    case title of
        Just t ->
            Element.htmlAttribute (Html.Attributes.title t)

        Nothing ->
            Element.htmlAttribute (Html.Attributes.classList [])


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
                + persona.numinousPoints
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
                        if availablePoints > 0 && value < 20 then
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
        , Theme.row
            [ alignBottom
            , Border.widthEach
                { left = 0
                , top = 1
                , bottom = 0
                , right = 0
                }
            , Element.paddingEach
                { left = 0
                , top = Theme.rhythm
                , bottom = 0
                , right = 0
                }
            , width fill
            ]
            [ text "Level Bonus"
            , el [ alignRight ] (text (String.fromInt (levelBonus persona)))
            ]
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
            [ viewPoints "EP" "Euphoria Points" persona.euphoriaPoints (usedEuphoriaPoints persona) <| \newValue -> { persona | euphoriaPoints = newValue }
            , viewPoints "IP" "Ichor Points" persona.ichorPoints (usedIchorPoints persona) <| \newValue -> { persona | ichorPoints = newValue }
            , viewPoints "NP" "Numinous Points" persona.numinousPoints (usedNuminousPoints persona) <| \newValue -> { persona | numinousPoints = newValue }
            ]
        ]


usedIchorPoints : Persona -> Int
usedIchorPoints persona =
    --  TODO
    0


usedEuphoriaPoints : Persona -> Int
usedEuphoriaPoints persona =
    persona.features
        |> List.map (\level -> level + 10)
        |> List.sum


usedNuminousPoints : Persona -> Int
usedNuminousPoints persona =
    max 0
        (persona.fitness
            + persona.grace
            + persona.ardor
            + persona.sanity
            + persona.prowess
            + persona.moxie
            - 18
        )


viewPoints : String -> String -> Int -> Int -> (Int -> Persona) -> Element Persona
viewPoints label fullName value used setter =
    let
        unused : Int
        unused =
            value - used
    in
    Theme.row [ width fill ]
        [ Theme.withHint fullName (text label)
        , Theme.wrappedRow [ width <| px (Theme.rhythm * 8) ]
            (List.repeat (unused // 5) (tallyGroup 5)
                ++ [ tallyGroup (modBy 5 unused) ]
            )
        , Theme.button [ alignRight ]
            { label = Icons.minus
            , onPress =
                if unused > 0 then
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
            , features =
                [ ( 1, prehensileProficiency )
                , ( 2, dominantExemplar )
                , ( 3, ambrosia )
                , ( 4, gardenKeeper )
                , ( 5, fairyFlight )
                ]
                    |> Dict.fromList
            , organs =
                [ prehensile "Sinuous Tentacle Tongue"
                , hands "Slender Elegant Hands"
                , breasts "Perky Marshmallow Tits"
                , hips "Tight Supple Ass"
                , phallic "Veiny Futa Phallus"
                , legs "Long Shapely Legs"
                ]
            }

        Flower ->
            { name = "The Flower"
            , description = "She is an object of tempting beauty and creative growth, decorative and useful in equal measure. Her body is offered freely to any who would take her, for her longing to be plucked and kept and tended by a worthy gardener runs deep. But she is fearless, and is not one who trades herself for safety. Only for joy."
            , features =
                -- TODO
                Dict.empty
            , organs =
                [ mouth "Soft Rosen Lips"
                , hands "Delicate Girly Hands"
                , breasts "Soft Succulent Boobies"
                , hips "Plush Bubble Butt"
                , yonic "Yielding Silken Quim"
                , legs "Cute Limber Legs"
                ]
            }

        Vixen ->
            { name = "The Vixen"
            , description = "She is one who embodies the predator, the explorer, and the protector all at once. Her whims and her primal hungers drive her to hunt and slake, to frolic and play, to guard and comfort, forever seeking excitement. But her mercurial heart is innocent and soft, gentle and always welcoming... just like her bosom."
            , features =
                -- TODO
                Dict.empty
            , organs =
                [ mouth "Hot Hungry Maw"
                , hands "Deft Nimble Hands"
                , breasts "Mamerous Milk Melons"
                , hips "Bouncy Runner Rump"
                , yonic "Juicy Nether Cleft"
                , legs "Beastly Hunter Legs"
                ]
            }

        Buck ->
            { name = "The Buck"
            , description = "He is one who lives in the moment and is captivated by passion. His earnest whimsy and innocent fascinations lead to carefree nights and a fondness for the unexpected, even for the dangerous, ever delighted by the thrill. But he yearns most deeply to be safe in the arms of someone stronger and kinder than himself."
            , features =
                -- TODO
                Dict.empty
            , organs =
                [ mouth "Pretty Princely Pout"
                , hands "Clever Flexible Fingers"
                , hips "Bitable Boy Butt"
                , phallic "Throbbing Meat Pole"
                , legs "Quick Springy Legs"
                ]
            }

        Fiend ->
            { name = "The Fiend"
            , description = "He is a cruel being of meticulous obsession and exacting desires. His esoteric pleasures are often strange or abstract, and he will craft them himself if he must, or if he prefers. But his implacable single-minded pursuit of his strange joys is intrinsically entwined with the intense empathy and fascination he feels for his living toys."
            , features =
                -- TODO
                Dict.empty
            , organs =
                [ mouth "Sensuous Knowing Lips"
                , hands "Steady Dexterous Hands"
                , hips "Chiseled Stately Ass"
                , phallic "Darkly Dreaming Dick"
                , legs "Fit Flexible Legs"
                ]
            }

        Doll ->
            { name = "The Doll"
            , description = "She is a blissful being of peaceful passivity and masochistic fatalism. Her only wish is to be treasured and tormented, teased and tantalized, in the hands of one worthy to own her, or even remake her. But her selfless wish to gratify her demanding master is tempered by her selfish wish for a life of mindless ecstasy."
            , features =
                -- TODO
                Dict.empty
            , organs =
                -- TODO
                [ hips "Pliable Femme Hips" ]
            }

        Custom record ->
            record


emptyOrgan : Organ
emptyOrgan =
    { name = ""
    , contour = 0
    , erogeny = 0
    , canSquish = False
    , canGrip = False
    , canPenetrate = False
    , canEnsheathe = False
    , isSquishable = False
    , isGrippable = False
    , isPenetrable = False
    , isEnsheatheable = False
    }


mouth : String -> Organ
mouth name =
    { emptyOrgan
        | name = name
        , contour = 1
        , erogeny = 2
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isPenetrable = True
    }


hands : String -> Organ
hands name =
    { emptyOrgan
        | name = name
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


breasts : String -> Organ
breasts name =
    { emptyOrgan
        | name = name
        , contour = 5
        , erogeny = 4
        , canSquish = True
        , isSquishable = True
        , isGrippable = True
    }


hips : String -> Organ
hips name =
    { emptyOrgan
        | name = name
        , contour = 1
        , erogeny = 4
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isGrippable = True
        , isPenetrable = True
    }


legs : String -> Organ
legs name =
    { emptyOrgan
        | name = name
        , contour = 0
        , erogeny = 1
        , canSquish = True
        , isGrippable = True
    }


phallic : String -> Organ
phallic name =
    { emptyOrgan
        | name = name
        , contour = 2
        , erogeny = 7
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


yonic : String -> Organ
yonic name =
    { emptyOrgan
        | name = name
        , contour = 3
        , erogeny = 6
        , canSquish = True
        , canEnsheathe = True
        , isSquishable = True
        , isPenetrable = True
    }


prehensile : String -> Organ
prehensile name =
    { emptyOrgan
        | name = name
        , contour = 4
        , erogeny = 2
        , canGrip = True
        , canPenetrate = True
        , isGrippable = True
        , isEnsheatheable = True
    }


prehensileProficiency : Feature
prehensileProficiency =
    { name = "Prehensile Proficiency"
    , description = "When using an Organ with both [CanGrip] and [CanPenetrate] to make a Prowess Roll, you may make the roll twice and take the superior result."
    }


dominantExemplar : Feature
dominantExemplar =
    { name = "Dominant Exemplar"
    , description = """You now have a pool of **Dominance Points** with capacity: **3**. Dominance Points do not persist between Encounters and begin at **0**.

You also permanently gain access to these three **Moves**:

> **Assertive Grope** (Tease) [Grips] | CT **6** |  
> If, and only if, the Stimulation dealt by this Move causes **0** Understimulation _and_ **0** Overstimulation, apply the **Subspace** effect to the target of this Move.

> **Wrecking Rut** (Thrust) [Penetrates] | CT **20** |  
If this Move deals Stimulation equal to or greater than the target's Sanity score, _and_ if the target of this Move has the **Subspace** effect, gain **1 Dominance Point**.

> **Plundering Plunge** (Thrust) [Penetrates] | CT **0** |  
> _You want that nectar, and it doesn't matter how deep you have to plunge in to taste it. Nowhere is safe from your tongue._
>
> If the Organ using this Move is your _Sinuous Tentacle Tongue_, add **+1** to this Move's attempted Stimulation, and also gain **1 Craving**.

During your partner's turn, you may spend **1 Dominance Point** to force them to take an action, or _not_ take an action, of your choice. You may only do this once per turn.

> **Subspace** _Passive_  
> You have disadvantage on all Grace Checks and Sanity Checks. You have advantage on all Ardor Checks and Moxie Checks.
>
> At the beginning of your turn, if you are not Having An Orgasm, you may roll a Moxie Check. If the result of the Check is greater than your Craving value, you may remove this effect.
"""
    }


ambrosia : Feature
ambrosia =
    { name = "Ambrosia"
    , description = """If your partner's mouth is Paired with your _Veiny Futa Phallus_ while you are **Having An Orgasm**, they may roll a **Sanity Check**. If the result of the Check is not greater than your penis' Contour, or if they choose not to make the Check, they compulsively swallow your ejaculate and acquire the **Fixation** effect.


> **Fixation** _Passive_  
> You have disadvantage on all actions that do not target the Organ that inflicted this effect. You cannot volitionally Unpair from the Organ that inflicted this effect.
>
> At the beginning of your turn, you may roll a Sanity Check. If the result of the Check is greater than your Craving value, you may remove this effect.

In addition, so long as the effect remains, you gain an extrasensory perception of their body and sexual state and may demand full access to all the information on their Persona Card and Organ Cards at any time.
"""
    }


gardenKeeper : Feature
gardenKeeper =
    -- TODO
    { name = "Garden Keeper"
    , description = """At the beginning of your turn, you may roll a **Fitness Check**. You may drain _up to_ that many points of **Craving** and add those points to your **Stamina Die**.

You also permanently gain access to these two Moves:

> ((Coming Soon / TBD))

> ((Coming Soon / TBD))"""
    }


fairyFlight : Feature
fairyFlight =
    { name = "Fairy Flight"
    , description = """Manifest at will ethereal butterfly wings that give you **+5** on all **Grace Check**s, both within and outside sexual encounters.

These wings count as two Occupied Appendages that support your weight and stabilize you, while they exist.

These wings also allow you to fly for a number of minutes equal to your Fitness score multiplied by 10. You recover flight-time at a rate of (1 + Fitness) minutes per minute of rest.
"""
    }


parseGendertrope : BitParser.Parser Gendertrope
parseGendertrope =
    BitParser.parseNonnegativeInt
        |> BitParser.andThen
            (\i ->
                case i of
                    0 ->
                        BitParser.map4
                            (\name description features organs ->
                                Custom
                                    { name = name
                                    , description = description
                                    , features = Dict.fromList features
                                    , organs = organs
                                    }
                            )
                            BitParser.parseString
                            BitParser.parseString
                            (BitParser.parseList
                                (BitParser.map2 Tuple.pair
                                    BitParser.parsePositiveInt
                                    parseFeature
                                )
                            )
                            (BitParser.parseList parseOrgan)

                    1 ->
                        BitParser.succeed Butterfly

                    2 ->
                        BitParser.succeed Flower

                    3 ->
                        BitParser.succeed Vixen

                    4 ->
                        BitParser.succeed Buck

                    5 ->
                        BitParser.succeed Fiend

                    6 ->
                        BitParser.succeed Doll

                    _ ->
                        BitParser.fail
            )


parseFeature : BitParser.Parser Feature
parseFeature =
    BitParser.map2 Feature
        BitParser.parseString
        BitParser.parseString


encodeGendertrope : Gendertrope -> List Bit
encodeGendertrope gendertrope =
    case gendertrope of
        Custom { name, description, features, organs } ->
            [ BitParser.encodeNonnegativeInt 0
            , BitParser.encodeString name
            , BitParser.encodeString description
            , BitParser.encodeList
                (\( k, v ) ->
                    [ BitParser.encodePositiveInt k
                    , BitParser.encodeString v.name
                    , BitParser.encodeString v.description
                    ]
                        |> List.concat
                )
                (Dict.toList features)
            , BitParser.encodeList encodeOrgan organs
            ]
                |> List.concat

        Butterfly ->
            BitParser.encodeNonnegativeInt 1

        Flower ->
            BitParser.encodeNonnegativeInt 2

        Vixen ->
            BitParser.encodeNonnegativeInt 3

        Buck ->
            BitParser.encodeNonnegativeInt 4

        Fiend ->
            BitParser.encodeNonnegativeInt 5

        Doll ->
            BitParser.encodeNonnegativeInt 6


encodeOrgan : Organ -> List Bit
encodeOrgan organ =
    [ BitParser.encodeString organ.name
    , BitParser.encodeNonnegativeInt organ.contour
    , BitParser.encodeNonnegativeInt organ.erogeny
    , BitParser.encodeBool organ.canSquish
    , BitParser.encodeBool organ.canGrip
    , BitParser.encodeBool organ.canPenetrate
    , BitParser.encodeBool organ.canEnsheathe
    , BitParser.encodeBool organ.isSquishable
    , BitParser.encodeBool organ.isGrippable
    , BitParser.encodeBool organ.isPenetrable
    , BitParser.encodeBool organ.isEnsheatheable
    ]
        |> List.concat


parseOrgan : BitParser.Parser Organ
parseOrgan =
    BitParser.succeed Organ
        |> BitParser.andMap BitParser.parseString
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseNonnegativeInt
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool
        |> BitParser.andMap BitParser.parseBool


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
    , BitParser.encodeList BitParser.encodePositiveInt persona.features
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
        |> BitParser.andMap (BitParser.parseList BitParser.parsePositiveInt)
