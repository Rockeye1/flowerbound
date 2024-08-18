module Persona.View exposing (Config, organs, persona, tallyGroup)

import Dict
import Element exposing (Element, alignBottom, alignRight, centerX, centerY, el, fill, height, padding, paragraph, px, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icons
import Persona
import Persona.Codec
import Persona.Data
import Persona.Types exposing (Feature, GendertropeRecord, Organ, Persona)
import Theme


type alias Config msg =
    { update : Persona -> msg
    , upload : msg
    }


persona : Config msg -> Persona -> Element msg
persona config input =
    Theme.column
        [ Border.width 1
        , Theme.padding
        , Font.color Theme.purple
        ]
        ([ Theme.input []
            { text = Persona.Codec.toUrl input
            , onChange =
                \newUrl ->
                    Persona.Codec.fromUrl newUrl
                        |> Result.withDefault input
                        |> config.update
            , placeholder = Nothing
            , label = Input.labelLeft [] (text "URL")
            }
         , Theme.row [ width fill ]
            (Persona.Data.gendertropeIcon input.gendertrope
                :: text input.name
                :: topButtons config
            )
         , Theme.row [ width fill ]
            [ abilitiesView input
            , statusView input
            ]
         , progressionView input
         ]
            ++ viewGendertrope input
        )


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
    Theme.column [ height fill, width fill ]
        [ text "Ability Scores"
        , Element.table [ Theme.spacing ]
            { data =
                [ ( Theme.withHint "Fitness" (text "FIT"), input.fitness )
                , ( Theme.withHint "Grace" (text "GRC"), input.grace )
                , ( Theme.withHint "Ardor" (text "ARD"), input.ardor )
                , ( Theme.withHint "Sanity" (text "SAN"), input.sanity )
                , ( Theme.withHint "Prowess" (text "PRW"), input.prowess )
                , ( Theme.withHint "Moxie" (text "MOX"), input.moxie )
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
        ]


statusView : Persona -> Element msg
statusView input =
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
                , statusRow "Max Satiation" input.ardor
                , statusRow "Max Craving" input.sanity
                , statusRow "Max Arousal" input.prowess
                , statusRow "Max Sensitivity" input.moxie
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
            , el [ alignRight ] (text (String.fromInt (Persona.levelBonus input)))
            ]
        ]


progressionView : Persona -> Element msg
progressionView input =
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
            [ viewPoints "EP" "Euphoria Points" input.euphoriaPoints (Persona.usedEuphoriaPoints input)
            , viewPoints "IP" "Ichor Points" input.ichorPoints (Persona.usedIchorPoints input)
            , viewPoints "NP" "Numinous Points" input.numinousPoints (Persona.usedNuminousPoints input)
            ]
        ]


viewPoints : String -> String -> Int -> Int -> Element msg
viewPoints label fullName value used =
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


viewGendertrope : Persona -> List (Element msg)
viewGendertrope ({ gendertrope } as input) =
    let
        gendertropeRecord : GendertropeRecord
        gendertropeRecord =
            Persona.Data.gendertropeToRecord gendertrope
    in
    [ Theme.row []
        [ Persona.Data.gendertropeIcon gendertrope
        , text gendertropeRecord.name
        ]
    , paragraph [ Font.italic ]
        [ text gendertropeRecord.description
        ]
    , organs gendertropeRecord.organs
    , viewStandardFeatures input.features gendertropeRecord
    ]


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
                (paragraph [ Font.underline ]
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
