module Frontend exposing (app)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element exposing (Element, alignRight, centerX, el, fill, height, shrink, text, width)
import Element.Border as Border
import Element.Input as Input
import Lamdera
import List.Extra
import Theme
import Types exposing (FrontendModel, FrontendMsg(..), Persona, ToFrontend(..))
import Url


app :
    { init : Lamdera.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
    , view : FrontendModel -> Browser.Document FrontendMsg
    , update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
    , subscriptions : FrontendModel -> Sub FrontendMsg
    , onUrlRequest : UrlRequest -> FrontendMsg
    , onUrlChange : Url.Url -> FrontendMsg
    }
app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view = view
        }


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ key =
    ( { key = key
      , personas =
            [ edmundDantes
            ]
      }
    , Cmd.none
    )


edmundDantes : Persona
edmundDantes =
    { name = "Edmund Dantes"
    , fitness = 2
    , grace = 2
    , ardor = 2
    , sanity = 2
    , prowess = 2
    , moxie = 2

    --
    , stamina = 1
    , satiation = 0
    , craving = 0
    , arousal = 0
    , sensitivity = 0

    --
    , euphoriaPoints = 0
    , ichorPoints = 0
    , numinousPoints = 0
    }


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        ChangePersona index persona ->
            ( { model | personas = List.Extra.setAt index persona model.personas }, Cmd.none )


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [] (innerView model)
        ]
    }


innerView : FrontendModel -> Element FrontendMsg
innerView { personas } =
    personas
        |> List.indexedMap (\index persona -> Element.map (ChangePersona index) (viewPersona persona))
        |> Theme.column [ Theme.padding ]


viewPersona : Persona -> Element Persona
viewPersona persona =
    let
        nameRow : Element Persona
        nameRow =
            Input.text [ width fill ]
                { label = Input.labelHidden "Name"
                , text = persona.name
                , onChange = \newValue -> { persona | name = newValue }
                , placeholder =
                    Just <|
                        Input.placeholder [] <|
                            text "Name"
                }
                |> el
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
                          , view = \( label, _, _ ) -> text label
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
                , text "Stamina"
                , text "Satiation"
                , text "Craving"
                , text "Arousal"
                , text "Sensitivity"
                ]

        progressionView : Element msg
        progressionView =
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
                , Theme.row [ width fill ]
                    [ el [ width fill ] <| text "EP"
                    , el [ width fill ] <| text "IP"
                    , el [ width fill ] <| text "NP"
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


subscriptions : FrontendModel -> Sub FrontendMsg
subscriptions _ =
    Sub.none
