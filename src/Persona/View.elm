module Persona.View exposing (Config, persona, tallyGroup, viewOrgans)

import Dict
import Icons
import Persona
import Persona.Codec
import Persona.Data as Data
import Phosphor exposing (IconVariant)
import Site
import Theme
import Types exposing (Action(..), Feature, GendertropeRecord, Organ, Persona)
import Ui exposing (Attribute, Element, alignRight, centerX, centerY, el, fill, height, padding, px, row, shrink, spacing, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout as Layout
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
            Data.gendertropeToRecord input.gendertrope

        colors : Persona.Colors
        colors =
            Persona.toColors input
    in
    Theme.column
        (Ui.border 1
            :: Ui.width (px 640)
            :: Ui.background colors.background
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
                :: Theme.input [ Font.color colors.accent ]
                    { text = Site.config.canonicalUrl ++ Persona.Codec.toUrl input
                    , onChange =
                        \newUrl ->
                            Persona.Codec.fromUrl newUrl
                                |> Result.withDefault input
                                |> config.update
                    , placeholder = Nothing
                    , label = label.id
                    }
                :: topButtons config colors
            )
        , Theme.row
            [ centerX
            , Font.color colors.accent
            ]
            [ Data.gendertropeIconElement input.gendertrope
            , el [ Font.bold ] (text input.name)
            , Data.gendertropeIconElement input.gendertrope
            ]
        , Theme.row [ centerX ]
            [ viewAbilities input
            , viewStatus input colors
            ]
        , Theme.row
            [ centerX
            , Font.color colors.accent
            ]
            [ Data.gendertropeIconElement input.gendertrope
            , el [ Font.bold ] (text gendertropeRecord.name)
            , Data.gendertropeIconElement input.gendertrope
            ]
        , paragraph [ Font.italic ] [ text gendertropeRecord.description ]
        , viewOrgans gendertropeRecord.organs colors
        , viewStandardFeatures input.features gendertropeRecord colors
        ]


viewAbilities : Persona -> Element msg
viewAbilities input =
    [ ( "Fitness (FIT)", input.fitness )
    , ( "Grace (GRC)", input.grace )
    , ( "Ardor (ARD)", input.ardor )
    , ( "Sanity (SAN)", input.sanity )
    , ( "Prowess (PRW)", input.prowess )
    , ( "Moxie (MOX)", input.moxie )
    ]
        |> List.concatMap
            (\( label, value ) ->
                [ text label
                , el [ Font.alignRight ] (text (String.fromInt value))
                ]
            )
        |> Layout.rowWithConstraints [ Layout.fill, Layout.byContent ] [ Theme.spacing ]


viewStatus : Persona -> Persona.Colors -> Element msg
viewStatus input colors =
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
        |> Layout.rowWithConstraints [ Layout.fill, Layout.byContent ]
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
            , Ui.borderColor colors.accent
            , Theme.spacing
            ]


tallyGroup : Int -> Element msg
tallyGroup count =
    if count <= 0 then
        Ui.none

    else
        row
            [ spacing (Theme.rhythm // 2)
            , width shrink
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
                        , width fill
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
    el
        [ Ui.border 1
        , width <| px 1
        , height <| px 16
        ]
        Ui.none


viewStandardFeatures : List Int -> GendertropeRecord -> Persona.Colors -> Element msg
viewStandardFeatures features gendertropeRecord colors =
    let
        viewStandardFeature : ( Int, Feature ) -> Element msg
        viewStandardFeature ( level, feature ) =
            Theme.column
                [ Ui.border 1
                , Theme.padding
                ]
                (paragraph
                    [ Font.color colors.accent
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


topButtons : Config msg -> Persona.Colors -> List (Element msg)
topButtons config colors =
    [ Theme.iconButton [ alignRight ]
        { onPress = Just config.upload
        , icon = Icons.upload
        , title = "Upload"
        , accentColor = colors.accent
        }
    , case config.remove of
        Nothing ->
            Ui.none

        Just remove ->
            Theme.iconButton [ alignRight ]
                { onPress = Just remove
                , icon = Icons.remove
                , title = "Remove"
                , accentColor = colors.accent
                }
    ]


viewOrgans : List Organ -> Persona.Colors -> Element msg
viewOrgans input colors =
    let
        wrap : Int -> List (Attribute msg) -> Element msg -> Element msg
        wrap index attrs child =
            el
                (height fill
                    :: Theme.padding
                    :: (if modBy 2 index == 0 then
                            Ui.background Theme.transparentLightGray

                        else
                            Ui.noAttr
                       )
                    :: attrs
                )
                (paragraph [ centerY ] [ child ])

        intColumn : (Organ -> Int) -> Organ -> Element msg
        intColumn prop organ =
            -- (el
            --     [ Font.color colors.accent
            --     , Font.size 24
            --     , centerX
            --     ]
            --     (text (intToDots (prop organ)))
            -- )
            el [ Font.center ] (text (String.fromInt (prop organ)))

        boolColumn : IconVariant -> (c -> Bool) -> c -> Element msg
        boolColumn img prop organ =
            if prop organ then
                el [ centerX, Font.color colors.accent ] (Icons.toElement img)

            else
                Ui.none

        boolHeader : String -> String -> Element msg
        boolHeader label hint =
            el [ padding (Theme.rhythm // 2) ]
                (Theme.withHint hint (text label))

        canColumn : Action -> (c -> Bool) -> c -> Element msg
        canColumn attribute getter =
            boolColumn
                (Types.actionToCanIcon attribute)
                getter

        canHeader : Action -> Element msg
        canHeader attribute =
            boolHeader
                ("C" ++ Types.actionToInitial attribute)
                (Types.actionToCan attribute)

        isColumn : Action -> (c -> Bool) -> c -> Element msg
        isColumn attribute getter =
            boolColumn
                (Types.actionToIsIcon attribute)
                getter

        isHeader : Action -> Element msg
        isHeader attribute =
            boolHeader
                ("I" ++ Types.actionToInitial attribute)
                (Types.actionToIs attribute)
    in
    input
        |> List.indexedMap
            (\index element ->
                [ text element.name
                , el [ centerX, Font.color colors.accent ] (Icons.toElement (Data.organTypeToIcon element.type_))
                , intColumn .contour element
                , intColumn .erogeny element
                , canColumn Squishes .canSquish element
                , canColumn Grips .canGrip element
                , canColumn Penetrates .canPenetrate element
                , canColumn Ensheathes .canEnsheathe element
                , isColumn Squishes .isSquishable element
                , isColumn Grips .isGrippable element
                , isColumn Penetrates .isPenetrable element
                , isColumn Ensheathes .isEnsheatheable element
                ]
                    |> List.map (wrap index [])
            )
        |> (::)
            [ el [] Ui.none
            , el [ padding (Theme.rhythm // 2) ] (Ui.text "Type")
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
        |> Layout.rowWithConstraints
            [ Layout.fill
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            , Layout.byContent
            ]
            [ centerX ]
