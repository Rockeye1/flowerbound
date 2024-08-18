module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Effect exposing (Effect)
import Element exposing (Element, alignRight, centerX, centerY, el, fill, shrink, text, width)
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
import Persona.Codec
import Persona.Types exposing (Persona)
import RouteBuilder exposing (StatefulRoute)
import Shared
import Site
import Theme
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


type Model
    = WaitingForPersona String
    | Playing PlayingModel


type alias PlayingModel =
    { persona : Persona
    , stimulationCost : Int
    , sensitivity : Int
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
                    ( Playing newModel, effect )

        PickedFile file ->
            ( model, Effect.ReadPersonaFromMarkdown file LoadedFromFile )


innerUpdate : PlayingMsg -> PlayingModel -> ( PlayingModel, Effect msg )
innerUpdate msg model =
    case msg of
        StimulationCost stimulationCost ->
            ( { model | stimulationCost = stimulationCost }, Effect.none )


initPlayingModel : Persona -> PlayingModel
initPlayingModel persona =
    { persona = persona
    , stimulationCost = 1
    , sensitivity = 0
    , arousal = 0
    , craving = 0
    , satiation = 0
    , stamina = 0
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
                (Theme.viewMarkdown cheatSheet
                    ++ [ viewPlaying playingModel
                            |> Element.map PlayingMsg
                       ]
                )
                    |> Theme.column [ Theme.padding ]
                    |> Element.map PagesMsg.fromMsg
    }


viewPlaying : PlayingModel -> Element PlayingMsg
viewPlaying model =
    let
        nameColumn : Element.Column ( String, Int -> String ) msg
        nameColumn =
            { width = shrink
            , header = Element.none
            , view =
                \( label, _ ) ->
                    el
                        [ Theme.padding
                        , Font.center
                        ]
                        (text label)
            }

        costColumn : Int -> Element.Column ( String, Int -> String ) PlayingMsg
        costColumn c =
            { width = shrink
            , header = Element.none
            , view =
                \( _, toValue ) ->
                    Theme.button
                        [ Font.center
                        , if c == model.stimulationCost then
                            Background.color Theme.purple

                          else
                            Background.color Theme.gray
                        , if c == model.stimulationCost then
                            Theme.noAttribute

                          else
                            Font.color Theme.black
                        ]
                        { onPress = Just (StimulationCost c)
                        , label = text (toValue c)
                        }
            }
    in
    Element.table []
        { data =
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
        , columns = nameColumn :: List.map costColumn (List.range 1 18)
        }


cheatSheet : String
cheatSheet =
    """## Temperaments

**Innocent**: You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.
- Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Craving** value, drain the value of the result from your **Sensitivity**.

**Thoughtful**: You are dwelling on the emotions and emotional implications and the shape of your future.
- When calculating the Aftermath of your turn, first roll a **Moxie Check**. If the result is _less_ than your current **Arousal** value, drain the value of the result from your **Satiation**.

**Perverse**: You are excited on a conceptual, kinky level, captivated and compelled.
- Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Sensitivity** value, add the result to your **Craving** value.

**Valiant**: You are proud of yourself for enduring, but you are enduring rather than enjoying.
- When calculating whether or not you are currently having an **Orgasm**, roll a **Moxie Check**. If the result is _less_ than your current **Stamina** value, add the result to your Orgasm Threshold as a Modifier.

## Orgasm
To determine if you are Having An Orgasm you first determine your **Orgasm Threshold** by adding your **Sensitivity** to your **Satiation** and then also adding Modifiers if there are any.

ORGASM THRESHOLD = SENSITIVITY + SATIATION (+ MODIFIERS)

Once you know your **Orgasm Threshold**, you simply compare it to your **Arousal**. If your **Arousal** is greater than your **Orgasm Threshold**, you are **Having An Orgasm**.

`AROUSAL > ORGASM THRESHOLD`

## Stimulation
Choose a stamina cost by clicking the table below."""
