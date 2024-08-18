module Persona exposing (Config, default, fromPartial, gendertropeToPartial, levelBonus, partialGendertropeName, toPartial, view)

import Dict
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, column, el, fill, height, padding, paragraph, px, row, shrink, spacing, text, width)
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
import Persona.Data
import Persona.Types exposing (Feature, Gendertrope(..), GendertropeRecord, Organ, PartialGendertrope(..), PartialPersona, Persona, standardGendertropes)
import Theme exposing (withHint)


default : Persona
default =
    { name = "Cinderella Sheen"

    --
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0

    --
    , gendertrope = Vixen
    , features = []
    }


type alias Config msg =
    { update : Persona -> msg, flip : msg }


levelBonus :
    { a
        | fitness : Int
        , grace : Int
        , ardor : Int
        , sanity : Int
        , prowess : Int
        , moxie : Int
        , features : List Int
    }
    -> Int
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
                                UpdateGendertrope newGendertrope ->
                                    config.update { persona | gendertrope = newGendertrope }

                                SelectFeatures newFeatures ->
                                    config.update { persona | features = newFeatures }
                        )
                    )
                    (viewGendertrope persona)
            )
        ]


type GendertropeMsg
    = SelectFeatures (List Int)
    | UpdateGendertrope Gendertrope


viewGendertrope : Persona -> List (Element GendertropeMsg)
viewGendertrope ({ gendertrope } as persona) =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            gendertropeToRecord gendertrope

        radioRow : Element GendertropeMsg
        radioRow =
            let
                common : List (Attribute msg)
                common =
                    [ Border.width 1
                    , Theme.padding
                    , Font.center
                    , width (Element.minimum 160 fill)
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
                                Theme.row [ width fill ]
                                    [ el [ width fill ] Element.none
                                    , Theme.row []
                                        [ gendertropeIcon option
                                        , case option of
                                            Custom _ ->
                                                text "Custom"

                                            _ ->
                                                text (gendertropeToRecord option).name
                                        ]
                                    , el [ width fill ] Element.none
                                    ]
                            , onPress = Just (UpdateGendertrope option)
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
                            |> UpdateGendertrope
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
                            |> UpdateGendertrope
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
                        |> UpdateGendertrope
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
            , Element.map SelectFeatures (viewStandardFeatures persona gendertropeRecord)
            ]


gendertropeIcon : Gendertrope -> Element GendertropeMsg
gendertropeIcon gendertrope =
    case gendertrope of
        Butterfly ->
            Icons.butterfly

        Flower ->
            Icons.flower

        Vixen ->
            Icons.vixen

        Buck ->
            Icons.buck

        Fiend ->
            Icons.fiend

        Doll ->
            Icons.doll

        Custom _ ->
            Icons.custom


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
        boolColumn label prop img =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    if prop organ then
                        wrap index (el [ centerX, Font.color Theme.purple ] img)

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
            , boolColumn "CS" .canSquish Icons.squish
            , boolColumn "CG" .canGrip Icons.grip
            , boolColumn "CP" .canPenetrate Icons.penetrate
            , boolColumn "CE" .canEnsheathe Icons.ensheathe
            , boolColumn "IS" .isSquishable Icons.squishable
            , boolColumn "IG" .isGrippable Icons.grippable
            , boolColumn "IP" .isPenetrable Icons.penetrable
            , boolColumn "IE" .isEnsheatheable Icons.ensheatheable
            , spacer
            ]
        }


viewOrgans : GendertropeRecord -> Element (List Organ)
viewOrgans gendertropeRecord =
    let
        wrap : Int -> Element Organ -> Element (List Organ)
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
                |> Element.map (\newOrgan -> List.Extra.setAt index newOrgan gendertropeRecord.organs)

        intColumn :
            String
            -> (Organ -> Int)
            -> (Int -> Organ -> Organ)
            -> Element.IndexedColumn Organ (List Organ)
        intColumn label prop setter =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    wrap index
                        (Input.text [ width <| px 60, Font.center ]
                            { text = String.fromInt (prop organ)
                            , onChange =
                                \newValue ->
                                    setter
                                        (Maybe.withDefault (prop organ) (String.toInt newValue))
                                        organ
                            , placeholder = Just <| Input.placeholder [] (text "0")
                            , label = Input.labelHidden label
                            }
                        )
            }

        boolColumn :
            String
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Element.IndexedColumn Organ (List Organ)
        boolColumn label prop setter =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (text label)
            , view =
                \index organ ->
                    wrap index
                        (el [ centerX, centerY ] <|
                            Input.checkbox []
                                { checked = prop organ
                                , label = Input.labelHidden label
                                , onChange = \newValue -> setter newValue organ
                                , icon = Theme.purpleCheckbox
                                }
                        )
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
              , view =
                    \index organ ->
                        wrap index
                            (Theme.input []
                                { text = organ.name
                                , placeholder = Just <| Input.placeholder [] (text "Name")
                                , label = Input.labelHidden "Name"
                                , onChange = \newName -> { organ | name = newName }
                                }
                            )
              }
            , intColumn "Cont." .contour <| \value organ -> { organ | contour = value }
            , intColumn "Erog." .erogeny <| \value organ -> { organ | erogeny = value }
            , boolColumn "CS" .canSquish <| \value organ -> { organ | canSquish = value }
            , boolColumn "CG" .canGrip <| \value organ -> { organ | canGrip = value }
            , boolColumn "CP" .canPenetrate <| \value organ -> { organ | canPenetrate = value }
            , boolColumn "CE" .canEnsheathe <| \value organ -> { organ | canEnsheathe = value }
            , boolColumn "IS" .isSquishable <| \value organ -> { organ | isSquishable = value }
            , boolColumn "IG" .isGrippable <| \value organ -> { organ | isGrippable = value }
            , boolColumn "IP" .isPenetrable <| \value organ -> { organ | isPenetrable = value }
            , boolColumn "IE" .isEnsheatheable <| \value organ -> { organ | isEnsheatheable = value }
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


viewFeatures : Persona -> GendertropeRecord -> Element GendertropeMsg
viewFeatures ({ features } as persona) gendertropeRecord =
    let
        viewFeature : ( Int, Feature ) -> Element GendertropeMsg
        viewFeature ( level, feature ) =
            let
                selected : Bool
                selected =
                    level == 1 || List.member level features

                canSelect : Bool
                canSelect =
                    selected || (persona.euphoriaPoints - usedEuphoriaPoints persona) >= 10 + level
            in
            Theme.row [ width fill ]
                [ Input.checkbox [ alignTop, width <| px 20 ]
                    { label =
                        Input.labelBelow
                            [ Element.rotate (degrees 90)
                            , if level == 1 || not canSelect then
                                Font.color Theme.gray

                              else
                                Font.color Theme.black
                            ]
                            (text "Select")
                    , checked = selected
                    , icon = Theme.purpleCheckbox
                    , onChange =
                        \_ ->
                            if level == 1 || not canSelect then
                                SelectFeatures features

                            else if selected then
                                SelectFeatures (List.Extra.remove level features)

                            else
                                SelectFeatures (level :: features)
                    }
                , Theme.column
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
                    (Theme.input [ width fill ]
                        { onChange = \newName -> { feature | name = newName }
                        , text = feature.name
                        , label = Input.labelLeft [] (text ("Level " ++ String.fromInt level ++ " Feature: "))
                        , placeholder = Just (Input.placeholder [] (text "Name"))
                        }
                        :: Theme.multiline [ width fill ]
                            { onChange = \newDescription -> { feature | description = newDescription }
                            , text = feature.description
                            , label = Input.labelHidden "Description"
                            , placeholder = Just (Input.placeholder [] (text "Description"))
                            , spellcheck = True
                            }
                        :: viewMarkdown feature.description
                    )
                    |> Element.map
                        (\newFeature ->
                            UpdateGendertrope
                                (Custom
                                    { gendertropeRecord
                                        | features =
                                            Dict.insert level newFeature gendertropeRecord.features
                                    }
                                )
                        )
                ]
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
                , Element.htmlAttribute (Html.Attributes.attribute "elm-pages:prefetch" "")
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
    , unorderedList =
        \items ->
            items
                |> List.map viewUnorderedListItem
                |> Theme.column []
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


viewUnorderedListItem : Markdown.Block.ListItem (Element msg) -> Element msg
viewUnorderedListItem (Markdown.Block.ListItem task children) =
    case task of
        Markdown.Block.NoTask ->
            row []
                [ el [ alignTop ] (text " - ")
                , paragraph [] children
                ]

        Markdown.Block.IncompleteTask ->
            row []
                [ el [ alignTop ] (text " [ ] ")
                , paragraph [] children
                ]

        Markdown.Block.CompletedTask ->
            row []
                [ el [ alignTop ] (text " [V] ")
                , paragraph [] children
                ]


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
                [ el [ alignRight ] (text (String.fromInt value))
                , Theme.button [ alignRight ]
                    { label = Icons.minus
                    , onPress =
                        if value > 2 then
                            Just (setter (value - 1))

                        else
                            Nothing
                    }
                , Theme.button [ alignRight ]
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
        statusRow : String -> Int -> ( String, Int )
        statusRow label bonusToCap =
            ( label
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
                [ statusRow "Max Stamina" 0
                , statusRow "Max Satiation" persona.ardor
                , statusRow "Max Craving" persona.sanity
                , statusRow "Max Arousal" persona.prowess
                , statusRow "Max Sensitivity" persona.moxie
                ]
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view = \( label, _ ) -> text label
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, maximum ) -> text (String.fromInt maximum)
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
            Persona.Data.butterfly

        Flower ->
            Persona.Data.flower

        Vixen ->
            Persona.Data.vixen

        Buck ->
            Persona.Data.buck

        Fiend ->
            Persona.Data.fiend

        Doll ->
            Persona.Data.doll

        Custom record ->
            record


fromPartial : String -> Maybe PartialPersona -> Maybe GendertropeRecord -> Persona
fromPartial name partialPersona maybeGendertrope =
    let
        defaulted : PartialPersona
        defaulted =
            case partialPersona of
                Just persona ->
                    persona

                Nothing ->
                    toPartial default

        gendertrope : Gendertrope
        gendertrope =
            partialGendertropeToGendertrope defaulted.gendertrope maybeGendertrope
    in
    { name = name

    --
    , fitness = defaulted.fitness
    , grace = defaulted.grace
    , ardor = defaulted.ardor
    , sanity = defaulted.sanity
    , prowess = defaulted.prowess
    , moxie = defaulted.moxie

    --
    , euphoriaPoints = defaulted.euphoriaPoints
    , ichorPoints = defaulted.ichorPoints
    , numinousPoints = defaulted.numinousPoints

    --
    , features = defaulted.features
    , gendertrope = gendertrope
    }


partialGendertropeToGendertrope : PartialGendertrope -> Maybe GendertropeRecord -> Gendertrope
partialGendertropeToGendertrope gendertrope maybeGendertrope =
    case gendertrope of
        PartialButterfly ->
            Butterfly

        PartialFlower ->
            Flower

        PartialVixen ->
            Vixen

        PartialBuck ->
            Buck

        PartialFiend ->
            Fiend

        PartialDoll ->
            Doll

        PartialCustom name ->
            maybeGendertrope
                |> Maybe.withDefault
                    { name = name
                    , description = ""
                    , features = Dict.empty
                    , organs = []
                    }
                |> Custom


toPartial : Persona -> PartialPersona
toPartial persona =
    { fitness = persona.fitness
    , grace = persona.grace
    , ardor = persona.ardor
    , sanity = persona.sanity
    , prowess = persona.prowess
    , moxie = persona.moxie

    --
    , euphoriaPoints = persona.euphoriaPoints
    , ichorPoints = persona.ichorPoints
    , numinousPoints = persona.numinousPoints

    --
    , features = persona.features
    , gendertrope = gendertropeToPartial persona.gendertrope
    }


gendertropeToPartial : Gendertrope -> PartialGendertrope
gendertropeToPartial gendertrope =
    case gendertrope of
        Butterfly ->
            PartialButterfly

        Flower ->
            PartialFlower

        Vixen ->
            PartialVixen

        Buck ->
            PartialBuck

        Fiend ->
            PartialFiend

        Doll ->
            PartialDoll

        Custom { name } ->
            PartialCustom name


partialGendertropeName : Persona.Types.PartialGendertrope -> String
partialGendertropeName partial =
    (partialGendertropeToGendertrope partial Nothing
        |> gendertropeToRecord
    ).name
