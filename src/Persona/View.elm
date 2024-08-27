module Persona.View exposing (Config, persona, tallyGroup, viewOrgans)

import Dict
import Icons
import Persona
import Persona.Codec
import Persona.Data
import Phosphor
import Site
import Theme
import Types exposing (Action(..), Feature, GendertropeRecord, Organ, Persona)
import Ui exposing (Attribute, Element, alignRight, centerX, centerY, el, fill, height, padding, px, row, shrink, spacing, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout
import Ui.Prose exposing (paragraph)
import Ui.Table


type alias Config msg =
    { update : Persona -> msg
    , upload : msg
    , remove : Maybe msg
    , persona : Persona
    }


persona : List (Attribute msg) -> Config msg -> Element msg
persona attrs config =
    let
        input : Persona
        input =
            config.persona

        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            Persona.Data.gendertropeToRecord input.gendertrope
    in
    Theme.column
        (Ui.border 1
            :: Theme.padding
            :: attrs
        )
        [ let
            label : { element : Element msg, id : Input.Label }
            label =
                Input.label "url" [ width shrink ] (text "URL")
          in
          Theme.row []
            (label.element
                :: Theme.input []
                    { text = Site.config.canonicalUrl ++ Persona.Codec.toUrl input
                    , onChange =
                        \newUrl ->
                            Persona.Codec.fromUrl newUrl
                                |> Result.withDefault input
                                |> config.update
                    , placeholder = Nothing
                    , label = label.id
                    }
                :: topButtons config
            )
        , Theme.row
            [ centerX
            , Font.color Theme.purple
            ]
            [ Persona.Data.gendertropeIconElement input.gendertrope
            , el [ Font.bold ] (text input.name)
            , Persona.Data.gendertropeIconElement input.gendertrope
            ]
        , Theme.row [ centerX ]
            [ viewAbilities input
            , viewStatus input
            ]
        , Theme.row
            [ centerX
            , Font.color Theme.purple
            ]
            [ Persona.Data.gendertropeIconElement input.gendertrope
            , el [ Font.bold ] (text gendertropeRecord.name)
            , Persona.Data.gendertropeIconElement input.gendertrope
            ]
        , paragraph
            [ Font.italic
            ]
            [ text gendertropeRecord.description
            ]
        , viewOrgans gendertropeRecord.organs
        , viewStandardFeatures input.features gendertropeRecord
        ]


viewAbilities : Persona -> Element msg
viewAbilities input =
    Theme.table []
        (Ui.Table.columns
            [ Ui.Table.column
                { header = Ui.Table.cell [ Ui.padding 0 ] Ui.none
                , view = \( label, _ ) -> Ui.Table.cell [ Ui.padding 0, centerY ] (text label)
                }
                |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }
            , Ui.Table.column
                { header = Ui.Table.cell [ Ui.padding 0 ] Ui.none
                , view = \( _, value ) -> Ui.Table.cell [ Ui.padding 0, Font.alignRight ] (text (String.fromInt value))
                }
            ]
        )
        [ ( "Fitness", input.fitness )
        , ( "Grace", input.grace )
        , ( "Ardor", input.ardor )
        , ( "Sanity", input.sanity )
        , ( "Prowess", input.prowess )
        , ( "Moxie", input.moxie )
        ]


viewStatus : Persona -> Element msg
viewStatus input =
    let
        statusRow : String -> Int -> ( String, Int )
        statusRow label bonusToCap =
            ( label
            , 20 + 2 * bonusToCap
            )
    in
    Theme.table
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
        ]
        (Ui.Table.columns
            [ Ui.Table.column
                { header = Ui.Table.cell [ Ui.padding 0 ] Ui.none
                , view = \( label, _ ) -> Ui.Table.cell [ Ui.padding 0 ] (text label)
                }
                |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }
            , Ui.Table.column
                { header = Ui.Table.cell [ Ui.padding 0 ] Ui.none
                , view = \( _, value ) -> Ui.Table.cell [ Ui.padding 0, Font.alignRight ] (text (String.fromInt value))
                }
            ]
        )
        [ statusRow "Max Stamina" 0
        , statusRow "Max Satiation" input.ardor
        , statusRow "Max Craving" input.sanity
        , statusRow "Max Arousal" input.prowess
        , statusRow "Max Sensitivity" input.moxie
        , ( "Level Bonus", Persona.levelBonus input )
        ]


tallyGroup : Int -> Element msg
tallyGroup count =
    if count <= 0 then
        Ui.none

    else
        row
            [ spacing (Theme.rhythm // 2)
            , Ui.inFront
                (if count == 5 then
                    el
                        [ Ui.borderWith
                            { bottom = 1
                            , top = 0
                            , right = 0
                            , left = 0
                            }
                        , centerY
                        , Ui.rotate (Ui.radians (degrees -10))
                        ]
                        Ui.none

                 else
                    Ui.none
                )
            , Ui.paddingXY (Theme.rhythm // 2) 0
            ]
            (List.repeat (min 4 count) tallyMark)


tallyMark : Element msg
tallyMark =
    el [ Ui.border 1, height <| px 16 ] Ui.none


viewStandardFeatures : List Int -> GendertropeRecord -> Element msg
viewStandardFeatures features gendertropeRecord =
    let
        viewStandardFeature : ( Int, Feature ) -> Element msg
        viewStandardFeature ( level, feature ) =
            Theme.column
                [ Ui.border 1
                , Theme.padding
                ]
                (paragraph
                    [ Font.color Theme.purple
                    , Font.underline
                    ]
                    [ text ("Level " ++ String.fromInt level ++ " Feature: ")
                    , el [ Font.bold ] (text feature.name)
                    ]
                    :: Theme.viewMarkdown feature.description
                )
    in
    gendertropeRecord.features
        |> Dict.toList
        |> List.filter (\( level, _ ) -> level == 1 || List.member level features)
        |> List.map viewStandardFeature
        |> Theme.column []


topButtons : Config msg -> List (Element msg)
topButtons config =
    [ Theme.iconButton [ alignRight ]
        { onPress = Just config.upload
        , icon = Icons.upload
        , title = "Upload"
        }
    , case config.remove of
        Nothing ->
            Ui.none

        Just remove ->
            Theme.iconButton [ alignRight ]
                { onPress = Just remove
                , icon = Icons.remove
                , title = "Remove"
                }
    ]


viewOrgans : List Organ -> Element msg
viewOrgans input =
    let
        wrap : Int -> List (Attribute msg) -> Element msg -> Ui.Table.Cell msg
        wrap index attrs child =
            Ui.Table.cell
                (height fill
                    :: padding (Theme.rhythm // 2)
                    :: Ui.background
                        (if modBy 2 index == 0 then
                            Theme.lightGray

                         else
                            Theme.white
                        )
                    :: attrs
                )
                (paragraph [ centerY ] [ child ])

        intColumn :
            String
            -> String
            -> (Organ -> Int)
            -> Ui.Table.Column globalState rowState Organ msg
        intColumn label hint prop =
            Ui.Table.columnWithState
                { header =
                    \_ ->
                        Ui.Table.cell [ padding (Theme.rhythm // 2) ]
                            (Theme.withHint hint (text label))
                , view =
                    \index _ organ ->
                        wrap index
                            [ Font.center ]
                            -- (el
                            --     [ Font.color Theme.purple
                            --     , Font.size 24
                            --     , centerX
                            --     ]
                            --     (text (intToDots (prop organ)))
                            -- )
                            (text (String.fromInt (prop organ)))
                }

        boolColumn :
            String
            -> String
            -> Phosphor.IconVariant
            -> (Organ -> Bool)
            -> Ui.Table.Column globalState rowState Organ msg
        boolColumn label hint img prop =
            Ui.Table.columnWithState
                { header =
                    \_ ->
                        Ui.Table.cell [ padding (Theme.rhythm // 2) ]
                            (Theme.withHint hint (text label))
                , view =
                    \index _ organ ->
                        if prop organ then
                            wrap index [ centerX, Font.color Theme.purple ] (Icons.toElement img)

                        else
                            wrap index [] Ui.none
                }

        spacer : Ui.Table.Column globalState rowState Organ msg
        spacer =
            Ui.Table.column
                { header = Ui.Table.cell [] Ui.none
                , view = \_ -> Ui.Table.cell [] Ui.none
                }
                |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }

        canColumn :
            Action
            -> (Organ -> Bool)
            -> Ui.Table.Column globalState rowState Organ msg
        canColumn attribute getter =
            boolColumn ("C" ++ Types.actionToInitial attribute)
                (Types.actionToCan attribute)
                (Types.actionToCanIcon attribute)
                getter

        isColumn :
            Action
            -> (Organ -> Bool)
            -> Ui.Table.Column globalState rowState Organ msg
        isColumn attribute getter =
            boolColumn ("I" ++ Types.actionToInitial attribute)
                (Types.actionToIs attribute)
                (Types.actionToIsIcon attribute)
                getter

        nameColumn : Ui.Table.Column globalState rowState Organ msg
        nameColumn =
            Ui.Table.columnWithState
                { header = \_ -> Ui.Table.cell [] Ui.none
                , view = \index _ { name } -> wrap index [] (text name)
                }
                |> Ui.Table.withWidth { fill = True, min = Nothing, max = Nothing }
    in
    Ui.Table.view []
        (Ui.Table.columns
            [ spacer
            , nameColumn
            , intColumn "Con" "Contour - how pleasing the Organ is to the sense of touch" .contour
            , intColumn "Ero" "Erogeny - how much of an erogenous zone that Organ is" .erogeny
            , canColumn Squishes .canSquish
            , canColumn Grips .canGrip
            , canColumn Penetrates .canPenetrate
            , canColumn Ensheathes .canEnsheathe
            , isColumn Squishes .isSquishable
            , isColumn Grips .isGrippable
            , isColumn Penetrates .isPenetrable
            , isColumn Ensheathes .isEnsheatheable
            , spacer
            ]
        )
        input
