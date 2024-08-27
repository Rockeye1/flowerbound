module Persona.View exposing (Config, persona, tallyGroup, viewOrgans)

import Dict
import Icons
import Persona
import Persona.Codec
import Persona.Data
import Site
import Theme
import Types exposing (Action(..), Feature, GendertropeRecord, Organ, Persona)
import Ui exposing (Attribute, Element, alignRight, centerX, centerY, el, fill, height, padding, px, row, shrink, spacing, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout
import Ui.Prose exposing (paragraph)


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
            :: Ui.width (px 470)
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
        , paragraph [ Font.italic ] [ text gendertropeRecord.description ]
        , viewOrgans gendertropeRecord.organs
        , viewStandardFeatures input.features gendertropeRecord
        ]


viewAbilities : Persona -> Element msg
viewAbilities input =
    [ ( "Fitness", input.fitness )
    , ( "Grace", input.grace )
    , ( "Ardor", input.ardor )
    , ( "Sanity", input.sanity )
    , ( "Prowess", input.prowess )
    , ( "Moxie", input.moxie )
    ]
        |> List.concatMap
            (\( label, value ) ->
                [ text label
                , el [ Font.alignRight ] (text (String.fromInt value))
                ]
            )
        |> Ui.Layout.rowWithConstraints [ Ui.Layout.fill, Ui.Layout.byContent ] [ Theme.spacing ]


viewStatus : Persona -> Element msg
viewStatus input =
    let
        statusRow : String -> Int -> ( String, Int )
        statusRow label bonusToCap =
            ( label
            , 20 + 2 * bonusToCap
            )
    in
    [ statusRow "Max Stamina" 0
    , statusRow "Max Satiation" input.ardor
    , statusRow "Max Craving" input.sanity
    , statusRow "Max Arousal" input.prowess
    , statusRow "Max Sensitivity" input.moxie
    , ( "Level Bonus", Persona.levelBonus input )
    ]
        |> List.concatMap
            (\( label, value ) ->
                [ text label
                , el [ Font.alignRight ] (text (String.fromInt value))
                ]
            )
        |> Ui.Layout.rowWithConstraints [ Ui.Layout.fill, Ui.Layout.byContent ]
            [ Ui.paddingWith
                { left = Theme.rhythm
                , top = 0
                , bottom = 0
                , right = 0
                }
            , Ui.borderWith
                { left = 1
                , top = 0
                , bottom = 0
                , right = 0
                }
            , Ui.borderColor Theme.purple
            , Theme.spacing
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
        wrap : Int -> List (Attribute msg) -> Element msg -> Element msg
        wrap index attrs child =
            el
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

        intColumn : (Organ -> Int) -> Organ -> Element msg
        intColumn prop organ =
            -- (el
            --     [ Font.color Theme.purple
            --     , Font.size 24
            --     , centerX
            --     ]
            --     (text (intToDots (prop organ)))
            -- )
            el [ Font.center ] (text (String.fromInt (prop organ)))

        boolColumn img prop index organ =
            if prop organ then
                wrap index [] (el [ centerX, Font.color Theme.purple ] (Icons.toElement img))

            else
                wrap index [] Ui.none

        boolHeader label hint =
            el [ padding (Theme.rhythm // 2) ]
                (Theme.withHint hint (text label))

        canColumn attribute getter =
            boolColumn
                (Types.actionToCanIcon attribute)
                getter

        canHeader attribute =
            boolHeader
                ("C" ++ Types.actionToInitial attribute)
                (Types.actionToCan attribute)

        isColumn attribute getter =
            boolColumn
                (Types.actionToIsIcon attribute)
                getter

        isHeader attribute =
            boolHeader
                ("I" ++ Types.actionToInitial attribute)
                (Types.actionToIs attribute)
    in
    input
        |> List.indexedMap
            (\index element ->
                [ wrap index [] (text element.name)
                , wrap index [] (intColumn .contour element)
                , wrap index [] (intColumn .erogeny element)
                , canColumn Squishes .canSquish index element
                , canColumn Grips .canGrip index element
                , canColumn Penetrates .canPenetrate index element
                , canColumn Ensheathes .canEnsheathe index element
                , isColumn Squishes .isSquishable index element
                , isColumn Grips .isGrippable index element
                , isColumn Penetrates .isPenetrable index element
                , isColumn Ensheathes .isEnsheatheable index element
                ]
            )
        |> (::)
            [ el [] Ui.none
            , el [ padding (Theme.rhythm // 2) ]
                (Theme.withHint "Contour - how pleasing the Organ is to the sense of touch" (text "Con"))
            , el [ padding (Theme.rhythm // 2) ]
                (Theme.withHint "Erogeny - how much of an erogenous zone that Organ is" (text "Ero"))
            , canHeader Squishes
            , canHeader Grips
            , canHeader Penetrates
            , canHeader Ensheathes
            , isHeader Squishes
            , isHeader Grips
            , isHeader Penetrates
            , isHeader Ensheathes
            ]
        |> List.concat
        |> Ui.Layout.rowWithConstraints
            [ Ui.Layout.fill
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            , Ui.Layout.byContent
            ]
            []
