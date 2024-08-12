module View.Persona exposing (view)

import Browser exposing (UrlRequest(..))
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, px, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Theme exposing (withHint)
import Types exposing (Msg(..), Persona)


view : Persona -> Element Persona
view persona =
    Theme.column
        [ Border.width 1
        , Theme.padding
        ]
        [ nameRow persona
        , Theme.row [ width fill ]
            [ abilitiesView persona
            , statusView persona
            ]
        , progressionView persona
        ]


nameRow : Persona -> Element Persona
nameRow persona =
    Input.text
        [ width fill
        , Font.color Theme.purple
        ]
        { label = Input.labelHidden "Name"
        , text = persona.name
        , onChange = \newValue -> { persona | name = newValue }
        , placeholder =
            Just <|
                Input.placeholder [] <|
                    text "Name"
        }
        |> Theme.el
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
                    { label = text "-"
                    , onPress =
                        if value > 2 then
                            Just (setter (value - 1))

                        else
                            Nothing
                    }
                , Theme.button []
                    { label = text "+"
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
            { label = text "-"
            , onPress =
                if value > 0 then
                    Just (setter (value - 1))

                else
                    Nothing
            }
        , Theme.button [ alignRight ]
            { label = text "+"
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
