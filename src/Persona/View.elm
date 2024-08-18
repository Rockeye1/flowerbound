module Persona.View exposing (Config, organs, persona, tallyGroup)

import Dict
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, padding, paragraph, px, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icons
import Persona
import Persona.Codec
import Persona.Data
import Persona.Types exposing (Feature, GendertropeRecord, Organ, Persona)
import Site
import Theme


type alias Config msg =
    { update : Persona -> msg
    , upload : msg
    }


persona : Config msg -> Persona -> Element msg
persona config input =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            Persona.Data.gendertropeToRecord input.gendertrope
    in
    Theme.column
        [ Border.width 1
        , Theme.padding
        , width <| Element.maximum 640 shrink
        ]
        [ Theme.row [ width fill ]
            (Theme.input [ width fill ]
                { text = Site.config.canonicalUrl ++ Persona.Codec.toUrl input
                , onChange =
                    \newUrl ->
                        Persona.Codec.fromUrl newUrl
                            |> Result.withDefault input
                            |> config.update
                , placeholder = Nothing
                , label = Input.labelLeft [] (text "URL")
                }
                :: topButtons config
            )
        , Theme.row
            [ centerX
            , Font.color Theme.purple
            ]
            [ Persona.Data.gendertropeIcon input.gendertrope
            , el [ Font.bold ] (text input.name)
            , Persona.Data.gendertropeIcon input.gendertrope
            ]
        , Theme.row [ centerX ]
            [ abilitiesView input
            , statusView input
            ]
        , Theme.row
            [ centerX
            , Font.color Theme.purple
            ]
            [ Persona.Data.gendertropeIcon input.gendertrope
            , el [ Font.bold ] (text gendertropeRecord.name)
            , Persona.Data.gendertropeIcon input.gendertrope
            ]
        , paragraph
            [ Font.italic
            , width fill
            ]
            [ text gendertropeRecord.description
            ]
        , organs gendertropeRecord.organs
        , viewStandardFeatures input.features gendertropeRecord
        ]


abilitiesView : Persona -> Element msg
abilitiesView input =
    let
        viewRow : ( Element msg, Int ) -> Element msg
        viewRow ( _, value ) =
            Theme.row
                [ alignRight ]
                [ el [ alignRight ] (text (String.fromInt value))
                ]
    in
    Element.table
        [ height fill
        , width fill
        , Theme.spacing
        ]
        { data =
            [ ( text "Fitness", input.fitness )
            , ( text "Grace", input.grace )
            , ( text "Ardor", input.ardor )
            , ( text "Sanity", input.sanity )
            , ( text "Prowess", input.prowess )
            , ( text "Moxie", input.moxie )
            ]
        , columns =
            [ { width = fill
              , header = Element.none
              , view = \( label, _ ) -> el [ centerY ] label
              }
            , { width = shrink
              , header = Element.none
              , view = viewRow
              }
            ]
        }


statusView : Persona -> Element msg
statusView input =
    let
        statusRow : String -> Int -> ( String, Int )
        statusRow label bonusToCap =
            ( label
            , 20 + 2 * bonusToCap
            )
    in
    Theme.table
        [ Theme.spacing
        , Border.widthEach
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
        { data =
            [ statusRow "Max Stamina" 0
            , statusRow "Max Satiation" input.ardor
            , statusRow "Max Craving" input.sanity
            , statusRow "Max Arousal" input.prowess
            , statusRow "Max Sensitivity" input.moxie
            , ( "Level Bonus", Persona.levelBonus input )
            ]
        , columns =
            [ { header = Element.none
              , width = fill
              , view = \( label, _ ) -> text label
              }
            , { header = Element.none
              , width = shrink
              , view = \( _, value ) -> el [ Font.alignRight ] (text (String.fromInt value))
              }
            ]
        }


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


viewStandardFeatures : List Int -> GendertropeRecord -> Element msg
viewStandardFeatures features gendertropeRecord =
    let
        viewStandardFeature : ( Int, Feature ) -> Element msg
        viewStandardFeature ( level, feature ) =
            Theme.column
                [ width fill
                , Border.width 1
                , Theme.padding
                , width fill
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
        |> Theme.column [ width fill ]


topButtons : Config msg -> List (Element msg)
topButtons config =
    [ Theme.button [ alignRight ]
        { onPress = Just config.upload
        , label = Icons.upload
        }
    ]


organs : List Organ -> Element msg
organs input =
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
        { data = input
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
