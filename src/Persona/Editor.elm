module Persona.Editor exposing (Config, GendertropeMsg(..), view)

import Dict
import Html
import Html.Attributes
import Html.Events
import Icons
import List.Extra
import Persona
import Persona.Data as Data
import Persona.View
import Phosphor
import Theme
import Types exposing (Action(..), Feature, Gendertrope(..), GendertropeRecord, Organ, OrganType(..), Persona)
import Ui exposing (Attribute, Element, alignBottom, alignRight, alignTop, centerX, centerY, el, fill, height, padding, px, shrink, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout as Layout
import Ui.Prose exposing (paragraph)
import Ui.Table as Table


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
        , centerX
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
                [ text "Gendertrope"
                , topButtons config
                ]
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
        [ el
            [ Font.color Theme.purple
            , width shrink
            ]
            (Data.gendertropeIconElement persona.gendertrope)
        , Theme.input []
            { label = Input.labelHidden "Name"
            , text = persona.name
            , onChange = \newValue -> config.update { persona | name = newValue }
            , placeholder = Just "Name"
            }
        , topButtons config
        ]


topButtons : Config msg -> Element msg
topButtons config =
    Theme.row [ alignRight ]
        [ Theme.iconButton []
            { onPress = Just config.upload
            , icon = Icons.upload
            , title = "Upload"
            }
        , Theme.iconButton []
            { onPress = Just config.download
            , icon = Icons.download
            , title = "Download"
            }
        , Theme.iconButton []
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

        viewRow : ( String, Int, Int -> Persona ) -> List (Element Persona)
        viewRow ( label, value, setter ) =
            [ el [ centerY ] (text label)
            , el [ Font.alignRight, centerY ] (text (String.fromInt value))
            , Theme.iconButton []
                { icon = Icons.minus
                , title = "Decrease"
                , onPress =
                    if value > 2 then
                        Just (setter (value - 1))

                    else
                        Nothing
                }
            , Theme.iconButton []
                { icon = Icons.plus
                , title = "Increase"
                , onPress =
                    if availablePoints > 0 && value < 20 then
                        Just (setter (value + 1))

                    else
                        Nothing
                }
            ]
    in
    Theme.column [ height fill ]
        [ text "Ability Scores"
        , [ ( "Fitness", persona.fitness, \newValue -> { persona | fitness = newValue } )
          , ( "Grace", persona.grace, \newValue -> { persona | grace = newValue } )
          , ( "Ardor", persona.ardor, \newValue -> { persona | ardor = newValue } )
          , ( "Sanity", persona.sanity, \newValue -> { persona | sanity = newValue } )
          , ( "Prowess", persona.prowess, \newValue -> { persona | prowess = newValue } )
          , ( "Moxie", persona.moxie, \newValue -> { persona | moxie = newValue } )
          ]
            |> List.concatMap viewRow
            |> Layout.rowWithConstraints [ Layout.fill, Layout.fill, Layout.byContent, Layout.byContent ] [ Theme.spacing ]
        ]


statusView : Persona -> Element msg
statusView persona =
    let
        statusRow : ( String, Persona -> Int ) -> List (Element msg)
        statusRow ( label, toCap ) =
            [ text label
            , text (String.fromInt (toCap persona))
            ]
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
        , [ ( "Max Stamina", Persona.maxStamina )
          , ( "Max Satiation", Persona.maxSatiation )
          , ( "Max Craving", Persona.maxCraving )
          , ( "Max Arousal", Persona.maxArousal )
          , ( "Max Sensitivity", Persona.maxSensitivity )
          ]
            |> List.concatMap statusRow
            |> Layout.rowWithConstraints [ Layout.fill, Layout.byContent ] [ Theme.spacing ]
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
        , Theme.wrappedRow []
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
            Data.gendertropeToRecord gendertrope

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
                        Theme.el
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
                            (Theme.row [ centerX ]
                                [ Data.gendertropeIconElement option
                                , case option of
                                    Custom { name } ->
                                        if
                                            List.any
                                                (\r -> (Data.gendertropeToRecord r).name == name)
                                                Types.standardGendertropes
                                        then
                                            text (name ++ " [Custom]")

                                        else
                                            text name

                                    _ ->
                                        text (Data.gendertropeToRecord option).name
                                ]
                            )
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
                        | organs =
                            newOrgans
                                |> List.Extra.removeWhen (\organ -> { organ | type_ = Other } == Data.other "")
                    }
                        |> Custom
                        |> UpdateGendertrope
                )
                (viewOrgans gendertropeRecord.organs)
            , viewFeatures persona gendertropeRecord
            ]

        Doll ->
            [ radioRow
            , paragraph [ Font.italic ]
                [ text gendertropeRecord.description
                ]
            , Ui.map
                (\newOrgans ->
                    { gendertropeRecord
                        | organs =
                            newOrgans
                                |> List.Extra.removeWhen (\organ -> { organ | type_ = Other } == Data.other "")
                    }
                        |> Custom
                        |> UpdateGendertrope
                )
                (viewOrgans gendertropeRecord.organs)
            , Ui.map SelectFeatures (viewStandardFeatures persona gendertropeRecord)
            ]

        _ ->
            [ radioRow
            , paragraph [ Font.italic ]
                [ text gendertropeRecord.description
                ]
            , Persona.View.viewOrgans gendertropeRecord.organs
            , Ui.map SelectFeatures (viewStandardFeatures persona gendertropeRecord)
            ]


viewOrgans : List Organ -> Element (List Organ)
viewOrgans organs =
    let
        wrap : Int -> Element Organ -> Table.Cell (List Organ)
        wrap index child =
            Table.cell
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
                    |> Ui.map
                        (\newOrgan ->
                            if index == List.length organs then
                                organs ++ [ newOrgan ]

                            else
                                List.Extra.setAt index newOrgan organs
                        )
                )

        intColumn :
            String
            -> String
            -> (Organ -> Int)
            -> (Int -> Organ -> Organ)
            -> Table.Column globalState rowState Organ (List Organ)
        intColumn label hint prop setter =
            Table.columnWithState
                { header =
                    \_ ->
                        Table.cell
                            [ padding (Theme.rhythm // 2)
                            , Font.center
                            ]
                            (Theme.withHint hint (text label))
                , view =
                    \index _ organ ->
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
            -> Table.Column globalState rowState Organ (List Organ)
        boolColumn label hint getter setter =
            Table.columnWithState
                { header =
                    \_ ->
                        Table.cell
                            [ padding (Theme.rhythm // 2)
                            , Font.center
                            ]
                            (Theme.withHint hint (text label))
                , view =
                    \index _ organ ->
                        wrap index
                            (el [ centerX, centerY ] <|
                                Theme.checkbox []
                                    { checked = getter organ
                                    , label = Input.labelHidden label
                                    , onChange = \newValue -> setter newValue organ
                                    }
                            )
                }

        canColumn :
            Action
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Table.Column globalState rowState Organ (List Organ)
        canColumn attribute getter setter =
            boolColumn ("C" ++ Types.actionToInitial attribute) (Types.actionToCan attribute) getter setter

        isColumn :
            Action
            -> (Organ -> Bool)
            -> (Bool -> Organ -> Organ)
            -> Table.Column globalState rowState Organ (List Organ)
        isColumn attribute getter setter =
            boolColumn ("I" ++ Types.actionToInitial attribute) (Types.actionToIs attribute) getter setter

        nameColumn : Table.Column globalState rowState Organ (List Organ)
        nameColumn =
            Table.columnWithState
                { header = \_ -> Table.cell [] Ui.none
                , view =
                    \index _ organ ->
                        wrap index
                            (Theme.input []
                                { text = organ.name
                                , placeholder = Just "Name"
                                , label = Input.labelHidden "Name"
                                , onChange = \newName -> { organ | name = newName }
                                }
                            )
                }

        typeColumn : Table.Column globalState rowState Organ (List Organ)
        typeColumn =
            Table.columnWithState
                { header =
                    \_ ->
                        Table.cell
                            [ padding (Theme.rhythm // 2)
                            , Font.center
                            ]
                            (text "Type")
                , view =
                    \index _ organ ->
                        let
                            popoverId : String
                            popoverId =
                                "organ-popover-" ++ String.fromInt index

                            organTypeButton : OrganType -> Html.Html Organ
                            organTypeButton type_ =
                                Html.button
                                    [ Html.Events.onClick { organ | type_ = type_ }
                                    , Html.Attributes.attribute "popovertarget" popoverId
                                    , Html.Attributes.attribute "popovertargetaction" "hide"
                                    ]
                                    [ Phosphor.toHtml [] (Data.organTypeToIcon type_)
                                    , Html.text (" " ++ Data.organTypeToString type_)
                                    ]
                        in
                        wrap index
                            (Theme.row
                                [ centerX
                                , centerY
                                ]
                                [ Theme.button
                                    [ centerY
                                    , Html.Attributes.attribute "popovertarget" popoverId
                                        |> Ui.htmlAttribute
                                    , Theme.title (Data.organTypeToString organ.type_)
                                    ]
                                    { onPress = Just organ
                                    , label = Icons.toElement (Data.organTypeToIcon organ.type_)
                                    }
                                , Html.div
                                    [ Html.Attributes.id popoverId
                                    , Html.Attributes.class "popover"
                                    , Html.Attributes.attribute "popover" ""
                                    ]
                                    (List.map organTypeButton Data.organTypes)
                                    |> Ui.html
                                ]
                            )
                }

        deleteColumn : Table.Column globalState rowState Organ (List Organ)
        deleteColumn =
            Table.columnWithState
                { header = \_ -> Table.cell [] Ui.none
                , view =
                    \index _ organ ->
                        wrap index
                            (if organ == Data.other "" then
                                Ui.none

                             else
                                Theme.button [ centerY ]
                                    { label = Icons.toElement Icons.delete
                                    , onPress = Just (Data.other "")
                                    }
                            )
                }
    in
    Table.view [ centerX ]
        (Table.columns
            [ nameColumn
            , typeColumn
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
            , deleteColumn
            ]
        )
        (organs ++ [ Data.other "" ])


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
                            , Ui.move (Ui.right 1)
                            , if level == 1 || not canSelect then
                                Font.color Theme.gray

                              else
                                Font.color Theme.black
                            ]
                            (text "Select")
                  in
                  el [ alignTop, width shrink ] <|
                    Theme.checkbox [ alignTop, Ui.below label.element ]
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
                                [ width shrink ]
                                (text ("Level " ++ String.fromInt level ++ " Feature: "))
                     in
                     [ Theme.row []
                        [ label.element
                        , Theme.input []
                            { onChange = \newName -> { feature | name = newName }
                            , text = feature.name
                            , label = label.id
                            , placeholder = Just "Name"
                            }
                        ]
                     , Theme.multiline []
                        { onChange = \newDescription -> { feature | description = newDescription }
                        , text = feature.description
                        , label = Input.labelHidden "Description"
                        , placeholder = Just "Description"
                        , spellcheck = True
                        }
                     , Theme.column [] (Theme.viewMarkdown feature.description)
                     ]
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
