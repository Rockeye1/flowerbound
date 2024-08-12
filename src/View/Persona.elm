module View.Persona exposing (view)

import Browser exposing (UrlRequest(..))
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, paragraph, px, rgb, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import Icons
import Theme exposing (withHint)
import Types exposing (Msg(..), Persona)


type alias Config msg =
    { update : Persona -> msg, flip : msg }


view :
    Config msg
    ->
        { flipped : Bool
        , persona : Persona
        }
    -> Element msg
view config { flipped, persona } =
    let
        commonAttrs flip =
            [ Border.width 1
            , width fill
            , height fill
            , Theme.padding
            , Background.color (rgb 1 1 1)
            , Element.htmlAttribute <| Html.Attributes.style "backface-visibility" "hidden"
            , Element.htmlAttribute <|
                Html.Attributes.style "transition" "all .5s ease-in-out"
            , Element.htmlAttribute <|
                Html.Attributes.style "transform"
                    ("rotateY("
                        ++ (if flip then
                                "180deg"

                            else
                                "0"
                           )
                        ++ ")"
                    )
            ]
    in
    el
        []
        (el
            [ Element.inFront <|
                Theme.column
                    (commonAttrs flipped)
                    [ nameRow config persona
                    , Element.map config.update <|
                        Theme.row [ width fill ]
                            [ abilitiesView persona
                            , statusView persona
                            ]
                    , Element.map config.update <| progressionView persona
                    ]
            ]
         <|
            Theme.column
                (commonAttrs (not flipped))
                [ Theme.row [ width fill ]
                    [ text "Gendertrope"
                    , Theme.button [ alignRight ]
                        { onPress = Just config.flip
                        , label = Icons.flip
                        }
                    ]
                , text "The Butterfly"
                , paragraph [ Font.italic ]
                    [ text "She is a creature of monstrous beauty and merciful power. Her amorous desires violate boundaries and overwhelm all resistance, rapacious and indomitable. But she is a nest-builder, a nurturer, one who cares for and cultivates that which her appetites have claimed as hers."
                    ]
                ]
        )


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
        [ Input.text
            [ width fill
            , Font.color Theme.purple
            ]
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
                [ text (String.fromInt value)
                , Theme.button []
                    { label = Icons.minus
                    , onPress =
                        if value > 2 then
                            Just (setter (value - 1))

                        else
                            Nothing
                    }
                , Theme.button []
                    { label = Icons.plus
                    , onPress =
                        if availablePoints > 0 then
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
        statusRow : String -> Int -> Int -> ( String, Int, Int )
        statusRow label value bonusToCap =
            ( label
            , value
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
                [ statusRow "Stamina" persona.stamina 0
                , statusRow "Satiation" persona.satiation persona.ardor
                , statusRow "Craving" persona.craving persona.sanity
                , statusRow "Arousal" persona.arousal persona.prowess
                , statusRow "Sensitivity" persona.sensitivity persona.moxie
                ]
            , columns =
                [ { header = Element.none
                  , width = fill
                  , view = \( label, _, _ ) -> text label
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, value, _ ) -> text (String.fromInt value)
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \_ -> text "/"
                  }
                , { header = Element.none
                  , width = shrink
                  , view = \( _, _, maximum ) -> text (String.fromInt maximum)
                  }
                ]
            }
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
            [ viewPoints "EP" "Euphoria Points" persona.euphoriaPoints <| \newValue -> { persona | euphoriaPoints = newValue }
            , viewPoints "IP" "Ichor Points" persona.ichorPoints <| \newValue -> { persona | ichorPoints = newValue }
            , viewPoints "NP" "Numinous Points" persona.numinousPoints <| \newValue -> { persona | numinousPoints = newValue }
            ]
        ]


viewPoints : String -> String -> Int -> (Int -> Persona) -> Element Persona
viewPoints label fullName value setter =
    Theme.row [ width fill ]
        [ Theme.withHint fullName (text label)
        , Theme.wrappedRow [ width <| px (Theme.rhythm * 8) ]
            (List.repeat (value // 5) (tallyGroup 5)
                ++ [ tallyGroup (modBy 5 value) ]
            )
        , Theme.button [ alignRight ]
            { label = Icons.minus
            , onPress =
                if value > 0 then
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
