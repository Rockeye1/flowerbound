module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Color
import Effect exposing (Effect)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, paragraph, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FatalError exposing (FatalError)
import File exposing (File)
import Head
import Head.Seo as Seo
import Icons
import MimeType
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.View
import RouteBuilder exposing (StatefulRoute)
import Shared
import Site
import Theme
import Types exposing (Attribute(..), Move, Persona, StimulationType(..))
import UrlPath exposing (UrlPath)
import View exposing (View)


type Msg
    = LoadFromUrl String
    | LoadFromFile
    | PlayingMsg PlayingMsg
    | PickedFile File
    | LoadedFromFile (Result String Persona)


type PlayingMsg
    = StimulationCost Int
    | UpdatePersona Persona
    | UpdateMeters Meters
    | SelectMove (Maybe String)
    | SelectTemperament (Maybe String)
    | BeginEncounter
    | Rest
    | RestedSatiation Int
    | RestedCraving Int


type Model
    = WaitingForPersona String
    | Playing PlayingModel


type alias PlayingModel =
    { persona : Persona
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe String
    }


type alias Meters =
    { sensitivity : Int
    , arousal : Int
    , craving : Int
    , satiation : Int
    , stamina : Int
    }


type alias RouteParams =
    {}


type alias Data =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


init : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect msg )
init _ _ =
    ( WaitingForPersona "", Effect.none )


update : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ _ msg model =
    case msg of
        LoadFromUrl url ->
            case Persona.Codec.fromUrl url of
                Err _ ->
                    case model of
                        WaitingForPersona _ ->
                            ( WaitingForPersona url, Effect.none )

                        Playing _ ->
                            ( model, Effect.none )

                Ok persona ->
                    case model of
                        WaitingForPersona _ ->
                            ( Playing (initPlayingModel persona), Effect.none )

                        Playing playingModel ->
                            ( Playing { playingModel | persona = persona }, Effect.none )

        LoadedFromFile (Ok persona) ->
            case model of
                WaitingForPersona _ ->
                    ( Playing (initPlayingModel persona), Effect.none )

                Playing playingModel ->
                    ( Playing { playingModel | persona = persona }, Effect.none )

        LoadedFromFile (Err _) ->
            ( model, Effect.none )

        LoadFromFile ->
            ( model, Effect.PickMarkdown PickedFile )

        PlayingMsg playingMsg ->
            case model of
                WaitingForPersona _ ->
                    ( model, Effect.none )

                Playing playingModel ->
                    let
                        ( newModel, effect ) =
                            innerUpdate playingMsg playingModel
                    in
                    ( Playing newModel, Effect.map PlayingMsg effect )

        PickedFile file ->
            ( model, Effect.ReadPersonaFromMarkdown file LoadedFromFile )


innerUpdate : PlayingMsg -> PlayingModel -> ( PlayingModel, Effect PlayingMsg )
innerUpdate msg model =
    let
        alterMeters : (Meters -> Meters) -> ( PlayingModel, Effect msg )
        alterMeters f =
            ( { model | meters = f model.meters }, Effect.none )
    in
    case msg of
        StimulationCost stimulationCost ->
            ( { model | stimulationCost = stimulationCost }, Effect.none )

        UpdatePersona persona ->
            ( { model | persona = persona }, Effect.none )

        UpdateMeters newMeters ->
            ( { model | meters = newMeters }, Effect.none )

        SelectMove selectedMove ->
            ( { model | selectedMove = selectedMove }, Effect.none )

        SelectTemperament selectedTemperament ->
            ( { model | selectedTemperament = selectedTemperament }, Effect.none )

        BeginEncounter ->
            alterMeters <| \meters -> { meters | stamina = 5 + model.persona.fitness }

        Rest ->
            ( { model
                | meters =
                    let
                        meters : Meters
                        meters =
                            model.meters
                    in
                    { meters
                        | arousal = 1
                        , sensitivity = 0
                    }
              }
            , Effect.batch
                [ Effect.rollCheck model.persona.ardor RestedSatiation
                , Effect.rollCheck model.persona.sanity RestedCraving
                ]
            )

        RestedSatiation satiation ->
            alterMeters <| \meters -> { meters | satiation = min (Persona.maxSatiation model.persona) satiation }

        RestedCraving craving ->
            alterMeters <| \meters -> { meters | craving = min (Persona.maxCraving model.persona) craving }


initPlayingModel : Persona -> PlayingModel
initPlayingModel persona =
    { persona = persona
    , stimulationCost = 1
    , meters =
        { sensitivity = 0
        , arousal = 0
        , craving = 0
        , satiation = 0
        , stamina = 0
        }
    , selectedMove = Nothing
    , selectedTemperament = Nothing
    }


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions _ _ _ _ =
    Sub.none


head : RouteBuilder.App Data ActionData RouteParams -> List Head.Tag
head _ =
    let
        image : Seo.Image
        image =
            { url = Pages.Url.fromPath [ "/android-chrome-192x192.png" ]
            , alt = "An orchid"
            , dimensions = Nothing
            , mimeType = Just (MimeType.Image MimeType.Png)
            }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = Site.manifest.name
        , image = image
        , description = Site.manifest.description
        , locale = Nothing
        , title = Site.manifest.name
        }
        |> Seo.website


data : BackendTask FatalError Data
data =
    BackendTask.succeed {}


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg Msg)
view _ _ model =
    { title = Site.manifest.name
    , body =
        case model of
            WaitingForPersona url ->
                Theme.column
                    [ Theme.padding
                    , centerX
                    , centerY
                    ]
                    [ Theme.row
                        [ Font.color Theme.purple
                        , centerX
                        , Font.center
                        ]
                        [ Icons.flower
                        , text "Welcome to Flowerbound"
                        , Icons.flower
                        ]
                    , Theme.el
                        [ Border.width 1
                        , Theme.padding
                        , width fill
                        ]
                        (Theme.input [ width <| Element.minimum 300 fill ]
                            { label = Input.labelAbove [] (text "URL")
                            , text = url
                            , onChange = LoadFromUrl
                            , placeholder = Just (Input.placeholder [] (text "Paste the Persona URL here"))
                            }
                        )
                    , el
                        [ centerX
                        , Font.color Theme.purple
                        ]
                        (text "or")
                    , Theme.row
                        [ Border.width 1
                        , Theme.padding
                        , width fill
                        ]
                        [ text "Load from a Markdown file"
                        , Theme.button [ alignRight ]
                            { onPress = Just LoadFromFile
                            , label = Icons.upload
                            }
                        ]
                    ]
                    |> Element.map PagesMsg.fromMsg

            Playing playingModel ->
                [ viewPersona playingModel
                , viewPlaying playingModel
                    |> Element.map PlayingMsg
                ]
                    |> Theme.column [ Theme.padding ]
                    |> Element.map PagesMsg.fromMsg
    }


viewPersona : PlayingModel -> Element Msg
viewPersona model =
    el [ centerX ] <|
        Persona.View.persona
            { update =
                \newPersona ->
                    newPersona
                        |> UpdatePersona
                        |> PlayingMsg
            , upload = LoadFromFile
            }
            model.persona


viewPlaying : PlayingModel -> Element PlayingMsg
viewPlaying ({ meters, persona } as model) =
    Theme.column [ width fill ]
        [ el [ Font.bold ] (text "Status meters")
        , Theme.row []
            [ Theme.button [ width fill ]
                { onPress = Just BeginEncounter
                , label = text "Begin Encounter"
                }
            , Theme.button [ width fill ]
                { onPress = Just Rest
                , label = text "Rest"
                }
            ]
        , Theme.table [ width fill ]
            { data =
                [ statusMeter "Stamina" meters.stamina (Persona.maxStamina persona) <| \newValue -> UpdateMeters { meters | stamina = newValue }
                , statusMeter "Satiation" meters.satiation (Persona.maxSatiation persona) <| \newValue -> UpdateMeters { meters | satiation = newValue }
                , statusMeter "Craving" meters.craving (Persona.maxCraving persona) <| \newValue -> UpdateMeters { meters | craving = newValue }
                , statusMeter "Sensitivity" meters.sensitivity (Persona.maxSensitivity persona) <| \newValue -> UpdateMeters { meters | sensitivity = newValue }
                , statusMeter "Arousal" meters.arousal (Persona.maxArousal persona) <| \newValue -> UpdateMeters { meters | arousal = newValue }
                ]
            , columns =
                [ { width = shrink
                  , header = Element.none
                  , view = Tuple.first
                  }
                , { width = fill
                  , header = Element.none
                  , view = Tuple.second
                  }
                ]
            }
        , el [ Font.bold ] (text "Orgasm")
        , viewOrgasm model
        , Theme.row [ width fill ]
            [ Theme.column
                [ width fill
                , alignTop
                ]
                [ el [ Font.bold ] (text "Moves")
                , text "Choose a move."
                , viewMoves model
                ]
            , Theme.column
                [ width fill
                , alignTop
                ]
                [ el [ Font.bold ] (text "Stimulation")
                , text "Choose a stamina cost."
                , staminaTable model
                ]
            ]
        , el [ Font.bold ] (text "Temperaments")
        , viewTemperaments model
        ]


viewOrgasm : PlayingModel -> Element PlayingMsg
viewOrgasm model =
    let
        modifiers : Int
        modifiers =
            -- TODO
            0

        orgasmThreshold : Int
        orgasmThreshold =
            model.meters.sensitivity + model.meters.satiation + modifiers
    in
    if model.meters.arousal > orgasmThreshold then
        paragraph
            [ Font.color Theme.white
            , Background.color Theme.purple
            , Theme.padding
            , width fill
            ]
            [ text "You are having an orgasm! (Unless your resist "
            , el [ Font.bold ] (text "Valiant")
            , text "ly)"
            ]

    else
        Theme.el
            [ Theme.padding
            , Border.width 1
            , width fill
            ]
            (text "You are not having an orgasm (yet!).")


viewTemperaments : PlayingModel -> Element PlayingMsg
viewTemperaments model =
    [ ( "Innocent", "You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Craving** value, drain the value of the result from your **Sensitivity**." )
    , ( "Thoughtful", "You are dwelling on the emotions and emotional implications and the shape of your future.", "When calculating the Aftermath of your turn, first roll a **Moxie Check**. If the result is _less_ than your current **Arousal** value, drain the value of the result from your **Satiation**." )
    , ( "Perverse", "You are excited on a conceptual, kinky level, captivated and compelled.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Sensitivity** value, add the result to your **Craving** value." )
    , ( "Valiant", "You are proud of yourself for enduring, but you are enduring rather than enjoying.", "When calculating whether or not you are currently having an **Orgasm**, roll a **Moxie Check**. If the result is _less_ than your current **Stamina** value, add the result to your Orgasm Threshold as a Modifier." )
    ]
        |> List.map (viewTemperament model)
        |> Theme.wrappedRow [ width fill ]


viewTemperament : PlayingModel -> ( String, String, String ) -> Element PlayingMsg
viewTemperament model ( name, description, consequence ) =
    let
        selected : Bool
        selected =
            model.selectedTemperament == Just name
    in
    Theme.selectableButton
        [ width <| Element.minimum 400 fill
        , height fill
        , Font.alignLeft
        ]
        { selected = selected
        , onPress =
            if selected then
                Just (SelectTemperament Nothing)

            else
                Just (SelectTemperament (Just name))
        , label =
            Theme.column [ alignTop ]
                (paragraph []
                    [ el [ Font.bold ] (text name)
                    , text " "
                    , text description
                    ]
                    :: Theme.viewMarkdown consequence
                )
        }


viewMoves : PlayingModel -> Element PlayingMsg
viewMoves model =
    (defaultMoves ++ featureMoves model.persona)
        |> List.map (viewMove model)
        |> Theme.column []


viewMove : PlayingModel -> Move -> Element PlayingMsg
viewMove model move =
    let
        selected : Bool
        selected =
            model.selectedMove == Just move.name
    in
    Theme.selectableButton
        [ width fill
        , Font.alignLeft
        ]
        { onPress =
            if selected then
                Just (SelectMove Nothing)

            else
                Just (SelectMove (Just move.name))
        , selected = selected
        , label =
            Theme.column [ width fill ]
                [ paragraph []
                    [ el [ Font.bold ] (text move.name)
                    , text
                        (" ("
                            ++ Types.stimulationTypeToString move.stimulationType
                            ++ ") ["
                            ++ String.join "/"
                                (List.map Types.attributeToString move.attributeCompatibility)
                            ++ "] | CT: "
                        )
                    , el [ Font.bold ] (text (String.fromInt move.cravingThreshold))
                    , text " |"
                    ]
                , paragraph []
                    [ text move.description
                    ]
                ]
        }


featureMoves : Persona -> List Move
featureMoves _ =
    -- TODO: implement this
    []


defaultMoves : List Move
defaultMoves =
    [ { name = "Caress"
      , stimulationType = Tease
      , attributeCompatibility = [ Squishes, Grips ]
      , cravingThreshold = 0
      , description = "A light touch with no other effects."
      }
    , { name = "Rub"
      , stimulationType = Grind
      , attributeCompatibility = [ Squishes, Grips, Penetrates, Ensheathes ]
      , cravingThreshold = 0
      , description = "A massaging motion with no other effects."
      }
    , { name = "Stroke"
      , stimulationType = Thrust
      , attributeCompatibility = [ Grips, Penetrates, Ensheathes ]
      , cravingThreshold = 0
      , description = "A back-and-forth movement with no other effects."
      }
    ]


statusMeter : String -> Int -> Int -> (Int -> msg) -> ( Element msg, Element msg )
statusMeter label value cap setter =
    ( el [ centerY ] (text label)
    , Theme.slider []
        { min = 0
        , max = cap
        , value = value
        , onChange = setter
        , label = label
        }
    )


staminaTable : PlayingModel -> Element PlayingMsg
staminaTable model =
    let
        nameColumn : Element.Column ( String, Int -> String ) msg
        nameColumn =
            { width = shrink
            , header = Element.none
            , view = \( label, _ ) -> el [ Theme.padding ] (text label)
            }

        costColumn : Int -> Element.Column ( String, Int -> String ) PlayingMsg
        costColumn cost =
            { width = shrink
            , header = Element.none
            , view =
                \( _, toValue ) ->
                    Theme.selectableButton []
                        { onPress = Just (StimulationCost cost)
                        , label = text (toValue cost)
                        , selected = cost == model.stimulationCost
                        }
            }

        rows : List ( String, Int -> String )
        rows =
            [ ( "Stamina Cost", \c -> String.fromInt c )
            , ( "Stimulation"
              , \c ->
                    if c == 1 then
                        "0"

                    else
                        String.fromInt (c * 2)
              )
            , ( "Dice type"
              , \c ->
                    if c == 1 then
                        "No Roll"

                    else
                        "d" ++ String.fromInt (c * 2)
              )
            ]
    in
    Theme.column [ centerX ]
        [ Theme.table [ spacing 4 ]
            { data = rows
            , columns = nameColumn :: List.map costColumn (List.range 1 6)
            }
        , Theme.table [ spacing 4 ]
            { data = rows
            , columns = nameColumn :: List.map costColumn (List.range 7 12)
            }
        , Theme.table [ spacing 4 ]
            { data = rows
            , columns = nameColumn :: List.map costColumn (List.range 13 18)
            }
        ]
