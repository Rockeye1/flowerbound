module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, centerY, el, fill, height, px, row, shrink, spacing, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Theme
import Types exposing (Flags, Model, Msg(..), Persona)
import Url


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { key = key
      , personas =
            [ cinderellaSheen
            ]
      }
    , Cmd.none
    )


cinderellaSheen : Persona
cinderellaSheen =
    { name = "Cinderella Sheen"
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , stamina = 0
    , satiation = 0
    , craving = 0
    , arousal = 0
    , sensitivity = 0

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        ChangePersona index persona ->
            ( { model | personas = List.Extra.setAt index persona model.personas }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = ""
    , body =
        [ Element.layout [] (innerView model)
        ]
    }


innerView : Model -> Element Msg
innerView { personas } =
    personas
        |> List.indexedMap (\index persona -> Element.map (ChangePersona index) (viewPersona persona))
        |> Theme.column [ Theme.padding ]


viewPersona : Persona -> Element Persona
viewPersona persona =
    let
        nameRow : Element Persona
        nameRow =
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

        splitView : Element Persona
        splitView =
            Theme.row []
                [ abilitiesView
                , statusView
                ]

        abilitiesView : Element Persona
        abilitiesView =
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
            in
            Theme.column [ height fill ]
                [ text "Ability Scores"
                , Element.table [ Theme.spacing ]
                    { data =
                        [ ( "Fitness (FIT) ", persona.fitness, \newValue -> { persona | fitness = newValue } )
                        , ( "Grace (GRC) ", persona.grace, \newValue -> { persona | grace = newValue } )
                        , ( "Ardor (ARD) ", persona.ardor, \newValue -> { persona | ardor = newValue } )
                        , ( "Sanity (SAN) ", persona.sanity, \newValue -> { persona | sanity = newValue } )
                        , ( "Prowess (PRW) ", persona.prowess, \newValue -> { persona | prowess = newValue } )
                        , ( "Moxie (MOX) ", persona.moxie, \newValue -> { persona | moxie = newValue } )
                        ]
                    , columns =
                        [ { width = fill
                          , header = Element.none
                          , view = \( label, _, _ ) -> el [ centerY ] <| text label
                          }
                        , { width = shrink
                          , header = Element.none
                          , view =
                                \( _, value, setter ) ->
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
                          }
                        ]
                    }
                ]

        statusView : Element Persona
        statusView =
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

        progressionView : Element Persona
        progressionView =
            let
                tallyMark : Element msg
                tallyMark =
                    el [ Border.width 1, height <| px 16 ] Element.none

                tallyGroup : Int -> Element msg
                tallyGroup count =
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

                viewTally : String -> Int -> (Int -> Persona) -> Element Persona
                viewTally label value setter =
                    Theme.row [ width fill ]
                        [ text label
                        , Theme.wrappedRow [ width fill ]
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
            in
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
                    [ viewTally "EP" persona.euphoriaPoints <| \newValue -> { persona | euphoriaPoints = newValue }
                    , viewTally "IP" persona.ichorPoints <| \newValue -> { persona | ichorPoints = newValue }
                    , viewTally "NP" persona.numinousPoints <| \newValue -> { persona | numinousPoints = newValue }
                    ]
                ]
    in
    Theme.column
        [ Border.width 1
        , Theme.padding
        ]
        [ nameRow
        , splitView
        , progressionView
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
