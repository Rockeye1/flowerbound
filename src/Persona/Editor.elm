module Persona.Editor exposing (Config, GendertropeMsg(..), view)

import Dict
import Element exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, paragraph, px, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Icons
import List.Extra
import Persona
import Persona.Data
import Persona.Types exposing (Feature, Gendertrope(..), GendertropeRecord, Organ, Persona)
import Persona.View
import Theme


type alias Config msg =
    { update : Persona -> msg
    , flip : msg
    , upload : msg
    , download : msg
    }


type GendertropeMsg
    = SelectFeatures (List Int)
    | UpdateGendertrope Gendertrope


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
                (text "Gendertrope"
                    :: topButtons config
                )
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
        (el [ Font.color Theme.purple ]
            (Persona.Data.gendertropeIcon persona.gendertrope)
            :: Theme.input [ width fill ]
                { label = Input.labelHidden "Name"
                , text = persona.name
                , onChange = \newValue -> config.update { persona | name = newValue }
                , placeholder =
                    Just <|
                        Input.placeholder [] <|
                            text "Name"
                }
            :: topButtons config
        )


topButtons : Config msg -> List (Element msg)
topButtons config =
    [ Theme.button [ alignRight ]
        { onPress = Just config.upload
        , label = Icons.upload
        }
    , Theme.button [ alignRight ]
        { onPress = Just config.download
        , label = Icons.download
        }
    , Theme.button [ alignRight ]
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
                [ ( Theme.withHint "Fitness" (text "FIT"), persona.fitness, \newValue -> { persona | fitness = newValue } )
                , ( Theme.withHint "Grace" (text "GRC"), persona.grace, \newValue -> { persona | grace = newValue } )
                , ( Theme.withHint "Ardor" (text "ARD"), persona.ardor, \newValue -> { persona | ardor = newValue } )
                , ( Theme.withHint "Sanity" (text "SAN"), persona.sanity, \newValue -> { persona | sanity = newValue } )
                , ( Theme.withHint "Prowess" (text "PRW"), persona.prowess, \newValue -> { persona | prowess = newValue } )
                , ( Theme.withHint "Moxie" (text "MOX"), persona.moxie, \newValue -> { persona | moxie = newValue } )
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
            , el [ alignRight ] (text (String.fromInt (Persona.levelBonus persona)))
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
            [ viewPoints "EP" "Euphoria Points" persona.euphoriaPoints (Persona.usedEuphoriaPoints persona) <| \newValue -> { persona | euphoriaPoints = newValue }
            , viewPoints "IP" "Ichor Points" persona.ichorPoints (Persona.usedIchorPoints persona) <| \newValue -> { persona | ichorPoints = newValue }
            , viewPoints "NP" "Numinous Points" persona.numinousPoints (Persona.usedNuminousPoints persona) <| \newValue -> { persona | numinousPoints = newValue }
            ]
        ]


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
            (List.repeat (unused // 5) (Persona.View.tallyGroup 5)
                ++ [ Persona.View.tallyGroup (modBy 5 unused) ]
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


viewGendertrope : Persona -> List (Element GendertropeMsg)
viewGendertrope ({ gendertrope } as persona) =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            Persona.Data.gendertropeToRecord gendertrope

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
            (Persona.Types.standardGendertropes ++ [ Custom gendertropeRecord ])
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
                                        [ Persona.Data.gendertropeIcon option
                                        , case option of
                                            Custom _ ->
                                                text "Custom"

                                            _ ->
                                                text (Persona.Data.gendertropeToRecord option).name
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
            , Persona.View.organs gendertropeRecord.organs
            , Element.map SelectFeatures (viewStandardFeatures persona gendertropeRecord)
            ]


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
                    selected || (persona.euphoriaPoints - Persona.usedEuphoriaPoints persona) >= 10 + level
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
                            :: Theme.viewMarkdown feature.description
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
                    selected || (persona.euphoriaPoints - Persona.usedEuphoriaPoints persona) >= 10 + level
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
                        :: Theme.viewMarkdown feature.description
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
