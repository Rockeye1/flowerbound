module Persona.View exposing (Config, persona, tallyGroup, viewOrgans)

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
import Phosphor
import Site
import Theme
import Types exposing (Attribute(..), Feature, GendertropeRecord, Organ, Persona)


type alias Config msg =
    { update : Persona -> msg
    , upload : msg
    , remove : Maybe msg
    , persona : Persona
    }


persona : List (Element.Attribute msg) -> Config msg -> Element msg
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
        (Border.width 1
            :: Theme.padding
            :: attrs
        )
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
            , width fill
            ]
            [ text gendertropeRecord.description
            ]
        , viewOrgans gendertropeRecord.organs
        , viewStandardFeatures input.features gendertropeRecord
        ]


viewAbilities : Persona -> Element msg
viewAbilities input =
    let
        viewRow : ( Element msg, Int ) -> Element msg
        viewRow ( _, value ) =
            Theme.row
                [ alignRight ]
                [ el [ alignRight ] (text (String.fromInt value))
                ]
    in
    Theme.table []
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
    [ Theme.iconButton [ alignRight ]
        { onPress = Just config.upload
        , icon = Icons.upload
        , title = "Upload"
        }
    , case config.remove of
        Nothing ->
            Element.none

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
        wrap : Int -> List (Element.Attribute msg) -> Element msg -> Element msg
        wrap index attrs child =
            el
                (width fill
                    :: height fill
                    :: padding (Theme.rhythm // 2)
                    :: Background.color
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
            -> Element.IndexedColumn Organ msg
        intColumn label hint prop =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (Theme.withHint hint (text label))
            , view =
                \index organ ->
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
            -> Element.IndexedColumn Organ msg
        boolColumn label hint img prop =
            { width = shrink
            , header = el [ padding (Theme.rhythm // 2) ] (Theme.withHint hint (text label))
            , view =
                \index organ ->
                    if prop organ then
                        wrap index [ centerX, Font.color Theme.purple ] (Icons.toElement img)

                    else
                        wrap index [] Element.none
            }

        spacer : Element.IndexedColumn organ msg
        spacer =
            { width = fill
            , header = Element.none
            , view = \_ _ -> Element.none
            }

        canColumn :
            Attribute
            -> (Organ -> Bool)
            -> Element.IndexedColumn Organ msg
        canColumn attribute getter =
            boolColumn ("C" ++ Types.attributeToInitial attribute) (Types.attributeToCan attribute) (Types.attributeToCanIcon attribute) getter

        isColumn :
            Attribute
            -> (Organ -> Bool)
            -> Element.IndexedColumn Organ msg
        isColumn attribute getter =
            boolColumn ("I" ++ Types.attributeToInitial attribute) (Types.attributeToIs attribute) (Types.attributeToIsIcon attribute) getter
    in
    Element.indexedTable [ width fill ]
        { data = input
        , columns =
            [ spacer
            , { width = fill
              , header = Element.none
              , view = \index { name } -> wrap index [] (text name)
              }
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
        }
