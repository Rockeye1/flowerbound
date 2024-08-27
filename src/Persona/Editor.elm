module Persona.Editor exposing (Config, GendertropeMsg(..), view)

import Dict
import Icons
import List.Extra
import Persona
import Persona.Data
import Persona.View
import Theme
import Types exposing (Action(..), Feature, Gendertrope(..), GendertropeRecord, Organ, Persona)
import Ui exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, px, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Prose exposing (paragraph)
import Ui.Table


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

        commonAttrs : Int -> List (Attribute msg)
        commonAttrs rotate =
            [ Ui.border 1
            , width <| px fullWidth
            , height fill
            , Theme.padding
            , Ui.background Theme.white
            , Theme.style "backface-visibility" "hidden"
            , Theme.style "transition" "all .5s ease-in-out"
            , Theme.style "position" "absolute"
            , Theme.style "transform"
                ("rotateY("
                    ++ String.fromInt rotate
                    ++ "deg)"
                )
            ]
    in
    Theme.row
        [ width <| px fullWidth
        , height <| px 600
        , Theme.style "perspective" "2000px"
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
            , Ui.map config.update <|
                Theme.row []
                    [ abilitiesView persona
                    , statusView persona
                    ]
            , Ui.map config.update <| progressionView persona
            ]
        , Theme.column
            (commonAttrs
                (if flipped then
                    360

                 else
                    180
                )
            )
            (Theme.row []
                (text "Gendertrope"
                    :: topButtons config
                )
                :: List.map
                    (Ui.map
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
        [ Ui.paddingWith
            { top = 0
            , bottom = Theme.rhythm
            , left = 0
            , right = 0
            }
        , Ui.borderWith
            { top = 0
            , right = 0
            , left = 0
            , bottom = 1
            }
        ]
        (el [ Font.color Theme.purple ]
            (Persona.Data.gendertropeIconElement persona.gendertrope)
            :: Theme.input []
                { label = Input.labelHidden "Name"
                , text = persona.name
                , onChange = \newValue -> config.update { persona | name = newValue }
                , placeholder = Just "Name"
                }
            :: topButtons config
        )


topButtons : Config msg -> List (Element msg)
topButtons config =
    [ Theme.iconButton [ alignRight ]
        { onPress = Just config.upload
        , icon = Icons.upload
        , title = "Upload"
        }
    , Theme.iconButton [ alignRight ]
        { onPress = Just config.download
        , icon = Icons.download
        , title = "Download"
        }
    , Theme.iconButton [ alignRight ]
        { onPress = Just config.flip
        , icon = Icons.flip
        , title = "Flip"
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

        viewRow : ( String, Int, Int -> Persona ) -> Ui.Table.Cell Persona
        viewRow ( _, value, setter ) =
            Ui.Table.cell []
                (Theme.row
                    [ alignRight ]
                    [ el [ alignRight ] (text (String.fromInt value))
                    , Theme.iconButton
                        [ alignRight
                        ]
                        { icon = Icons.minus
                        , title = "Decrease"
                        , onPress =
                            if value > 2 then
                                Just (setter (value - 1))

                            else
                                Nothing
                        }
                    , Theme.iconButton
                        [ alignRight
                        ]
                        { icon = Icons.plus
                        , title = "Increase"
                        , onPress =
                            if availablePoints > 0 && value < 20 then
                                Just (setter (value + 1))

                            else
                                Nothing
                        }
                    ]
                )
    in
    Theme.column [ height fill ]
        [ text "Ability Scores"
        , Theme.table []
            (Ui.Table.columns
                [ Ui.Table.column
                    { header = Ui.Table.cell [] Ui.none
                    , view = \( label, _, _ ) -> Ui.Table.cell [ centerY ] (text label)
                    }
                    |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }
                , Ui.Table.column
                    { header = Ui.Table.cell [] Ui.none
                    , view = viewRow
                    }
                ]
            )
            [ ( "Fitness", persona.fitness, \newValue -> { persona | fitness = newValue } )
            , ( "Grace", persona.grace, \newValue -> { persona | grace = newValue } )
            , ( "Ardor", persona.ardor, \newValue -> { persona | ardor = newValue } )
            , ( "Sanity", persona.sanity, \newValue -> { persona | sanity = newValue } )
            , ( "Prowess", persona.prowess, \newValue -> { persona | prowess = newValue } )
            , ( "Moxie", persona.moxie, \newValue -> { persona | moxie = newValue } )
            ]
        ]


statusView : Persona -> Element Persona
statusView persona =
    let
        statusRow : String -> (Persona -> Int) -> ( String, Int )
        statusRow label toCap =
            ( label
            , toCap persona
            )
    in
    Theme.column
        [ Ui.borderWith
            { left = 1
            , top = 0
            , bottom = 0
            , right = 0
            }
        , Ui.paddingWith
            { left = Theme.rhythm
            , top = 0
            , bottom = 0
            , right = 0
            }
        , height fill
        ]
        [ text "Status meters"
        , Theme.table []
            (Ui.Table.columns
                [ Ui.Table.column
                    { header = Ui.Table.cell [] Ui.none
                    , view = \( label, _ ) -> Ui.Table.cell [] (text label)
                    }
                    |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }
                , Ui.Table.column
                    { header = Ui.Table.cell [] Ui.none
                    , view = \( _, maximum ) -> Ui.Table.cell [] (text (String.fromInt maximum))
                    }
                ]
            )
            [ statusRow "Max Stamina" Persona.maxStamina
            , statusRow "Max Satiation" Persona.maxSatiation
            , statusRow "Max Craving" Persona.maxCraving
            , statusRow "Max Arousal" Persona.maxArousal
            , statusRow "Max Sensitivity" Persona.maxSensitivity
            ]
        , Theme.row
            [ alignBottom
            , Ui.borderWith
                { left = 0
                , top = 1
                , bottom = 0
                , right = 0
                }
            , Ui.paddingWith
                { left = 0
                , top = Theme.rhythm
                , bottom = 0
                , right = 0
                }
            ]
            [ text "Level Bonus"
            , el [ alignRight ] (text (String.fromInt (Persona.levelBonus persona)))
            ]
        ]


progressionView : Persona -> Element Persona
progressionView persona =
    Theme.column
        [ Ui.borderWith
            { top = 1
            , left = 0
            , right = 0
            , bottom = 0
            }
        , Ui.paddingWith
            { top = Theme.rhythm
            , left = 0
            , right = 0
            , bottom = 0
            }
        ]
        [ el [ centerX ] <| text "Progression Tally"
        , Theme.wrappedRow []
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
    Theme.row []
        [ Theme.withHint fullName (text label)
        , Theme.wrappedRow [ width <| px (Theme.rhythm * 8) ]
            (List.repeat (unused // 5) (Persona.View.tallyGroup 5)
                ++ [ Persona.View.tallyGroup (modBy 5 unused) ]
            )
        , Theme.iconButton [ alignRight ]
            { icon = Icons.minus
            , title = "Decrease"
            , onPress =
                if unused > 0 then
                    Just (setter (value - 1))

                else
                    Nothing
            }
        , Theme.iconButton [ alignRight ]
            { icon = Icons.plus
            , title = "Increase"
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
                common : Gendertrope -> List (Attribute GendertropeMsg)
                common option =
                    [ Ui.border 1
                    , Theme.padding
                    , Font.center
                    , Ui.widthMin 160
                    , Input.button (UpdateGendertrope option)
                    ]
            in
            (Types.standardGendertropes ++ [ Custom gendertropeRecord ])
                |> List.map
                    (\option ->
                        Theme.row
                            (if option == gendertrope then
                                Ui.background Theme.purple
                                    :: Font.color Theme.white
                                    :: Ui.borderColor Theme.black
                                    :: common option

                             else
                                Ui.background Theme.gray
                                    :: Ui.borderColor Theme.purple
                                    :: common option
                            )
                            [ el [] Ui.none
                            , Theme.row []
                                [ Persona.Data.gendertropeIconElement option
                                , case option of
                                    Custom _ ->
                                        text "Custom"

                                    _ ->
                                        text (Persona.Data.gendertropeToRecord option).name
                                ]
                            , el [] Ui.none
                            ]
                    )
                |> Theme.wrappedRow []
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
                , placeholder = Just "Name"
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
                , placeholder = Just "Description"
                , text = gendertropeRecord.description
                , spellcheck = True
                }
            , Ui.map
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
            , Persona.View.viewOrgans gendertropeRecord.organs
            , Ui.map SelectFeatures (viewStandardFeatures persona gendertropeRecord)
            ]


viewOrgans : GendertropeRecord -> Element (List Organ)
viewOrgans gendertropeRecord =
    let
        wrap : Int -> Element Organ -> Ui.Table.Cell (List Organ)
        wrap index child =
            Ui.Table.cell
                [ height fill
                , padding (Theme.rhythm // 2)
                , Ui.background
                    (if modBy 2 index == 0 then
                        Theme.lightGray

                     else
                        Theme.white
                    )
                ]
                (child
                    |> Ui.map (\newOrgan -> List.Extra.setAt index newOrgan gendertropeRecord.organs)
                )

        intColumn :
            String
            -> String
            -> (Organ -> Int)
            -> (Int -> Organ -> Organ)
            -> Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        intColumn label hint prop setter =
            Ui.Table.column
                { header = Ui.Table.cell [ padding (Theme.rhythm // 2) ] (Theme.withHint hint (text label))
                , view =
                    \( index, organ ) ->
                        wrap index
                            (Input.text [ width <| px 60, Font.center ]
                                { text = String.fromInt (prop organ)
                                , onChange =
                                    \newValue ->
                                        setter
                                            (Maybe.withDefault (prop organ) (String.toInt newValue))
                                            organ
                                , placeholder = Just "0"
                                , label = Input.labelHidden label
                                }
                            )
                }

        boolColumn :
            String
            -> String
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        boolColumn label hint getter setter =
            Ui.Table.column
                { header = Ui.Table.cell [ padding (Theme.rhythm // 2) ] (Theme.withHint hint (text label))
                , view =
                    \( index, organ ) ->
                        wrap index
                            (el [ centerX, centerY ] <|
                                Theme.checkbox []
                                    { checked = getter organ
                                    , label = Input.labelHidden label
                                    , onChange = \newValue -> setter newValue organ
                                    }
                            )
                }

        spacer : Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        spacer =
            Ui.Table.column
                { header = Ui.Table.cell [] Ui.none
                , view = \_ -> Ui.Table.cell [] Ui.none
                }
                |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }

        canColumn :
            Action
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        canColumn attribute getter setter =
            boolColumn ("C" ++ Types.actionToInitial attribute) (Types.actionToCan attribute) getter setter

        isColumn :
            Action
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        isColumn attribute getter setter =
            boolColumn ("I" ++ Types.actionToInitial attribute) (Types.actionToIs attribute) getter setter

        nameColumn : Ui.Table.Column globalState rowState ( Int, Organ ) (List Organ)
        nameColumn =
            Ui.Table.column
                { header = Ui.Table.cell [] Ui.none
                , view =
                    \( index, organ ) ->
                        wrap index
                            (Theme.input []
                                { text = organ.name
                                , placeholder = Just "Name"
                                , label = Input.labelHidden "Name"
                                , onChange = \newName -> { organ | name = newName }
                                }
                            )
                }
    in
    Ui.Table.view []
        (Ui.Table.columns
            [ spacer
            , nameColumn
            , intColumn "Cont" "Contour - how pleasing the Organ is to the sense of touch" .contour <| \value organ -> { organ | contour = value }
            , intColumn "Erog" "Erogeny - how much of an erogenous zone that Organ is" .erogeny <| \value organ -> { organ | erogeny = value }
            , canColumn Squishes .canSquish <| \value organ -> { organ | canSquish = value }
            , canColumn Grips .canGrip <| \value organ -> { organ | canGrip = value }
            , canColumn Penetrates .canPenetrate <| \value organ -> { organ | canPenetrate = value }
            , canColumn Ensheathes .canEnsheathe <| \value organ -> { organ | canEnsheathe = value }
            , isColumn Squishes .isSquishable <| \value organ -> { organ | isSquishable = value }
            , isColumn Grips .isGrippable <| \value organ -> { organ | isGrippable = value }
            , isColumn Penetrates .isPenetrable <| \value organ -> { organ | isPenetrable = value }
            , isColumn Ensheathes .isEnsheatheable <| \value organ -> { organ | isEnsheatheable = value }
            , spacer
            ]
        )
        (List.indexedMap Tuple.pair gendertropeRecord.organs)


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
                , Ui.border 0
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
                        [ Ui.border 1
                        , Theme.padding
                        , Ui.background
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
        |> Theme.column []


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
            Theme.row []
                [ let
                    label : { element : Element msg, id : Input.Label }
                    label =
                        Input.label
                            ("select-" ++ String.fromInt level)
                            [ Ui.rotate (Ui.turns 0.25)
                            , if level == 1 || not canSelect then
                                Font.color Theme.gray

                              else
                                Font.color Theme.black
                            ]
                            (text "Select")
                  in
                  Theme.column
                    []
                    [ Theme.checkbox [ alignTop, width <| px 20 ]
                        { label = label.id
                        , checked = selected
                        , onChange =
                            \_ ->
                                if level == 1 || not canSelect then
                                    SelectFeatures features

                                else if selected then
                                    SelectFeatures (List.Extra.remove level features)

                                else
                                    SelectFeatures (level :: features)
                        }
                    , label.element
                    ]
                , Theme.column
                    [ Ui.border 1
                    , Theme.padding
                    , Ui.background
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
                    (let
                        label : { element : Element msg, id : Input.Label }
                        label =
                            Input.label
                                ("level-" ++ String.fromInt level ++ "-feature")
                                []
                                (text ("Level " ++ String.fromInt level ++ " Feature: "))
                     in
                     Theme.row []
                        [ label.element
                        , Theme.input []
                            { onChange = \newName -> { feature | name = newName }
                            , text = feature.name
                            , label = label.id
                            , placeholder = Just "Name"
                            }
                        ]
                        :: Theme.multiline []
                            { onChange = \newDescription -> { feature | description = newDescription }
                            , text = feature.description
                            , label = Input.labelHidden "Description"
                            , placeholder = Just "Description"
                            , spellcheck = True
                            }
                        :: Theme.viewMarkdown feature.description
                    )
                    |> Ui.map
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
        |> Theme.column []
