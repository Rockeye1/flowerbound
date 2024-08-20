module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Color
import Dict exposing (Dict)
import Effect exposing (Effect)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, paragraph, shrink, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FatalError exposing (FatalError)
import File exposing (File)
import Head
import Head.Seo as Seo
import Icons
import Json.Decode
import List.Extra
import MimeType
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Data
import Persona.View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import RouteBuilder exposing (StatefulRoute)
import Set exposing (Set)
import Shared
import Site
import Theme
import Triple
import TypedSvg
import TypedSvg.Attributes
import TypedSvg.Attributes.InPx
import TypedSvg.Core
import TypedSvg.Events
import TypedSvg.Types
import Types exposing (Attribute(..), Move, Organ, Persona, StimulationType(..))
import UrlPath exposing (UrlPath)
import Vector2d exposing (Vector2d)
import View exposing (View)
import VirtualDom


type Msg
    = LoadFromUrl String
    | LoadFromFile
    | PlayingMsg PlayingMsg
    | PickedFile File
    | LoadedFromFile (Result String Persona)


type PlayingMsg
    = StimulationCost Int
    | UpdatePersona Persona
    | UpdateFromFile
    | UpdateOther Int Persona
    | UpdateOtherFromFile Int
    | AddFromFile
    | AddFromUrl String
    | UpdateMeters Meters
    | SelectMove (Maybe String)
    | SelectTemperament (Maybe String)
    | BeginEncounter
    | Rest
    | RestedSatiation Int
    | RestedCraving Int
    | RollValiantModifier
    | RolledValiantModifier Int
    | RollStimulation
    | RolledStimulation (List ( Int, Int ))
    | PickedUpdate File
    | ReadUpdate (Result String Persona)
    | PickedAdd File
    | ReadAdd (Result String Persona)
    | PickedUpdateOther Int File
    | ReadUpdateOther Int (Result String Persona)
    | MouseDown (Point2d Pixels ())
    | MouseMove (Point2d Pixels ())
    | MouseUp


type Model
    = WaitingForPersona
    | Playing PlayingModel


type alias PlayingModel =
    { persona : Persona
    , others : List Persona
    , organsPositions : Dict ( Int, String ) ( Point2d Pixels (), Int )
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe String
    , valiantModifier : Int
    , stimulationRoll : Maybe (List ( Int, Int ))
    , dragging : Maybe ( ( Int, String ), Vector2d Pixels () )
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
    ( WaitingForPersona, Effect.none )


update : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update _ _ msg model =
    case msg of
        LoadFromUrl url ->
            case Persona.Codec.fromUrl url of
                Err _ ->
                    -- TODO
                    ( model, Effect.none )

                Ok persona ->
                    ( Playing (initPlayingModel persona), Effect.none )

        LoadedFromFile (Ok persona) ->
            ( Playing (initPlayingModel persona), Effect.none )

        LoadedFromFile (Err e) ->
            let
                _ =
                    Debug.log e ()
            in
            -- TODO
            ( model, Effect.none )

        LoadFromFile ->
            ( model, Effect.PickMarkdown PickedFile )

        PlayingMsg playingMsg ->
            case model of
                WaitingForPersona ->
                    ( model, Effect.none )

                Playing playingModel ->
                    let
                        ( newModel, effect ) =
                            innerUpdate playingMsg playingModel
                    in
                    ( Playing (checkOrgans newModel), Effect.map PlayingMsg effect )

        PickedFile file ->
            ( model, Effect.ReadPersonaFromMarkdown file LoadedFromFile )


checkOrgans : PlayingModel -> PlayingModel
checkOrgans model =
    let
        expected : Set ( Int, String )
        expected =
            (model.persona :: model.others)
                |> List.indexedMap
                    (\index persona ->
                        persona.gendertrope
                            |> Persona.Data.gendertropeToRecord
                            |> .organs
                            |> List.map (\{ name } -> ( index - 1, name ))
                    )
                |> List.concat
                |> Set.fromList

        filtered : Dict ( Int, String ) ( Point2d Pixels (), Int )
        filtered =
            Dict.filter
                (\key _ -> Set.member key expected)
                model.organsPositions
    in
    { model
        | organsPositions =
            Set.foldl
                (\( i, organ ) ( acc, pos, z ) ->
                    case Dict.get ( i, organ ) acc of
                        Nothing ->
                            ( Dict.insert ( i, organ ) ( Point2d.pixels pos pos, z ) acc
                            , pos + 10
                            , z + 1
                            )

                        Just _ ->
                            ( acc, pos, z )
                )
                ( filtered
                , 10
                , getNewZOrder filtered
                )
                expected
                |> Triple.first
    }


innerUpdate : PlayingMsg -> PlayingModel -> ( PlayingModel, Effect PlayingMsg )
innerUpdate msg model =
    let
        alterMeters : (Meters -> Meters) -> ( PlayingModel, Effect msg )
        alterMeters f =
            ( { model | meters = f model.meters }, Effect.none )
    in
    case msg of
        StimulationCost stimulationCost ->
            ( { model
                | stimulationCost = stimulationCost
                , stimulationRoll = Nothing
              }
            , Effect.none
            )

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

        RollValiantModifier ->
            ( model, Effect.rollCheck model.persona.moxie RolledValiantModifier )

        RolledValiantModifier modifier ->
            ( { model | valiantModifier = modifier }, Effect.none )

        RollStimulation ->
            case
                List.Extra.findMap
                    (\( cost, dice ) ->
                        if cost == model.stimulationCost then
                            Just dice

                        else
                            Nothing
                    )
                    stimulationDice
                    |> Maybe.withDefault []
            of
                [] ->
                    ( { model | stimulationRoll = Nothing }, Effect.none )

                dice ->
                    ( { model | stimulationRoll = Nothing }, Effect.RollStimulation dice RolledStimulation )

        RolledStimulation stimulationRoll ->
            ( { model | stimulationRoll = Just stimulationRoll }, Effect.none )

        UpdatePersona persona ->
            ( { model | persona = persona }, Effect.none )

        UpdateFromFile ->
            ( model, Effect.PickMarkdown PickedUpdate )

        PickedUpdate file ->
            ( model, Effect.ReadPersonaFromMarkdown file ReadUpdate )

        ReadUpdate (Ok persona) ->
            ( { model | persona = persona }, Effect.none )

        ReadUpdate (Err _) ->
            -- TODO
            ( model, Effect.none )

        AddFromFile ->
            ( model, Effect.PickMarkdown PickedAdd )

        PickedAdd file ->
            ( model, Effect.ReadPersonaFromMarkdown file ReadAdd )

        ReadAdd (Ok persona) ->
            ( { model | others = model.others ++ [ persona ] }, Effect.none )

        ReadAdd (Err _) ->
            -- TODO
            ( model, Effect.none )

        AddFromUrl url ->
            case Persona.Codec.fromUrl url of
                Err _ ->
                    -- TODO
                    ( model, Effect.none )

                Ok persona ->
                    ( { model | others = model.others ++ [ persona ] }, Effect.none )

        UpdateOther index persona ->
            ( { model | others = List.Extra.setAt index persona model.others } |> checkOrgans, Effect.none )

        UpdateOtherFromFile index ->
            ( model, Effect.PickMarkdown (PickedUpdateOther index) )

        PickedUpdateOther index file ->
            ( model, Effect.ReadPersonaFromMarkdown file (ReadUpdateOther index) )

        ReadUpdateOther index (Ok persona) ->
            ( { model | others = List.Extra.setAt index persona model.others }, Effect.none )

        ReadUpdateOther _ (Err _) ->
            -- TODO
            ( model, Effect.none )

        MouseDown position ->
            case raycast model position of
                Nothing ->
                    let
                        _ =
                            Debug.log "raycast failed" ()
                    in
                    ( model, Effect.none )

                Just ( key, delta ) ->
                    ( { model | dragging = Just ( key, delta ) }, Effect.none )

        MouseMove position ->
            case model.dragging of
                Nothing ->
                    ( model, Effect.none )

                Just ( key, delta ) ->
                    let
                        zOrder : Int
                        zOrder =
                            getNewZOrder model.organsPositions
                    in
                    ( { model
                        | organsPositions =
                            Dict.insert key
                                ( Point2d.translateBy delta position
                                    |> clipOrganPosition
                                , zOrder
                                )
                                model.organsPositions
                      }
                    , Effect.none
                    )

        MouseUp ->
            ( { model | dragging = Nothing }, Effect.none )


clipOrganPosition : Point2d Pixels () -> Point2d Pixels ()
clipOrganPosition position =
    let
        { x, y } =
            Point2d.toPixels position
    in
    Point2d.pixels (clamp 0 (svgWidth - organWidth) x) (clamp 0 (svgHeight - organHeight) y)


getNewZOrder : Dict ( Int, String ) ( Point2d Pixels (), Int ) -> Int
getNewZOrder organsPositions =
    Dict.foldl
        (\_ ( _, z ) acc -> max (z + 1) acc)
        0
        organsPositions


raycast : PlayingModel -> Point2d Pixels () -> Maybe ( ( Int, String ), Vector2d Pixels () )
raycast model position =
    model.organsPositions
        |> Dict.toList
        |> List.sortBy (\( _, ( _, zOrder ) ) -> -zOrder)
        |> List.Extra.findMap
            (\( key, ( organPosition, _ ) ) ->
                let
                    vec : Vector2d Pixels ()
                    vec =
                        Vector2d.from position organPosition

                    { x, y } =
                        Vector2d.toPixels vec
                in
                if x <= 0 && x >= -organWidth && y <= 0 && y >= -organHeight then
                    Just ( key, vec )

                else
                    Nothing
            )


initPlayingModel : Persona -> PlayingModel
initPlayingModel persona =
    { persona = persona
    , others = []
    , stimulationCost = 1
    , organsPositions = Dict.empty
    , meters =
        { sensitivity = 0
        , arousal = 0
        , craving = 0
        , satiation = 0
        , stamina = 0
        }
    , selectedMove = Nothing
    , selectedTemperament = Nothing
    , valiantModifier = 0
    , stimulationRoll = Nothing
    , dragging = Nothing
    }
        |> checkOrgans


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
        (case model of
            WaitingForPersona ->
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
                    , loadPersona
                        { loadFromFile = LoadFromFile
                        , loadFromUrl = LoadFromUrl
                        }
                    ]

            Playing playingModel ->
                [ Theme.wrappedRow []
                    (Persona.View.persona
                        [ alignTop ]
                        { update = UpdatePersona
                        , upload = UpdateFromFile
                        , persona = playingModel.persona
                        }
                        :: List.indexedMap
                            (\i other ->
                                Persona.View.persona
                                    [ alignTop ]
                                    { update = UpdateOther i
                                    , upload = UpdateOtherFromFile i
                                    , persona = other
                                    }
                            )
                            playingModel.others
                        ++ [ Theme.column []
                                [ Theme.row
                                    [ Font.color Theme.purple
                                    , centerX
                                    , Font.center
                                    ]
                                    [ Icons.flower
                                    , text "Add another player"
                                    , Icons.flower
                                    ]
                                , loadPersona
                                    { loadFromFile = AddFromFile
                                    , loadFromUrl = AddFromUrl
                                    }
                                ]
                           ]
                    )
                , viewPlaying playingModel
                ]
                    |> Theme.column [ Theme.padding ]
                    |> Element.map PlayingMsg
        )
            |> Element.map PagesMsg.fromMsg
    }


loadPersona :
    { loadFromUrl : String -> msg
    , loadFromFile : msg
    }
    -> Element msg
loadPersona config =
    Theme.column [ centerX ]
        [ Theme.el
            [ Border.width 1
            , Theme.padding
            , width fill
            ]
            (Theme.input [ width <| Element.minimum 240 fill ]
                { label = Input.labelAbove [] (text "URL")
                , text = ""
                , onChange = config.loadFromUrl
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
                { onPress = Just config.loadFromFile
                , label = Icons.upload
                }
            ]
        ]


viewPlaying : PlayingModel -> Element PlayingMsg
viewPlaying ({ meters, persona } as model) =
    Theme.column [ width fill ]
        [ viewOrgans model
        , el [ Font.bold ] (text "Status meters")
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
                [ alignTop
                , width fill
                ]
                [ el [ Font.bold ] (text "Moves")
                , text "Choose a move."
                , viewMoves model
                ]
            , Theme.column [ alignTop ]
                [ el [ Font.bold ] (text "Stimulation")
                , text "Choose a stamina cost."
                , staminaTable model
                , Theme.row [ width fill ]
                    [ case model.stimulationRoll of
                        Nothing ->
                            Element.none

                        Just results ->
                            let
                                ( ardents, timids ) =
                                    List.unzip results

                                raw : Int
                                raw =
                                    List.sum ardents - List.sum timids

                                corrected : Int
                                corrected =
                                    if raw < 0 then
                                        min 0 (raw + model.persona.prowess)

                                    else if raw == 0 then
                                        0

                                    else
                                        max 0 (raw - model.persona.prowess)
                            in
                            paragraph []
                                [ text
                                    "Rolled "
                                , results
                                    |> List.map
                                        (\( ardent, timid ) ->
                                            String.fromInt ardent
                                                ++ " - "
                                                ++ String.fromInt timid
                                        )
                                    |> String.join " + "
                                    |> text
                                , text " = "
                                , text (String.fromInt raw)
                                , if raw == corrected then
                                    Element.none

                                  else
                                    text (" ⇒ " ++ String.fromInt corrected ++ " (PRW = " ++ String.fromInt model.persona.prowess ++ ")")
                                ]
                    , Theme.button [ alignRight ]
                        { onPress = Just RollStimulation
                        , label =
                            case model.stimulationRoll of
                                Nothing ->
                                    text "Roll"

                                Just _ ->
                                    text "Reroll"
                        }
                    ]
                ]
            ]
        , Theme.column []
            [ el [ Font.bold ] (text "Temperaments")
            , text "(Optionally) choose a Temperament"
            , viewTemperaments model
            ]
        ]


viewOrgans : PlayingModel -> Element PlayingMsg
viewOrgans model =
    Theme.column [ width fill ]
        [ el [ Font.bold ] (text "Organs")
        , model.organsPositions
            |> Dict.toList
            |> List.sortBy (\( _, ( _, zOrder ) ) -> zOrder)
            |> List.concatMap
                (\( ( i, organName ), ( pos, _ ) ) ->
                    let
                        maybePersona : Maybe Persona
                        maybePersona =
                            if i < 0 then
                                Just model.persona

                            else
                                List.Extra.getAt i model.others
                    in
                    case maybePersona of
                        Nothing ->
                            []

                        Just persona ->
                            case
                                persona.gendertrope
                                    |> Persona.Data.gendertropeToRecord
                                    |> .organs
                                    |> List.Extra.find (\organ -> organ.name == organName)
                            of
                                Nothing ->
                                    []

                                Just organ ->
                                    [ viewOrgan pos organ ]
                )
            |> (::) (TypedSvg.style [] [ TypedSvg.Core.text """svg text { cursor: default; }""" ])
            |> TypedSvg.svg
                [ TypedSvg.Attributes.width (TypedSvg.Types.percent 100)
                , TypedSvg.Attributes.viewBox 0 0 svgWidth svgHeight
                , TypedSvg.Events.on "mousedown" (VirtualDom.Custom (positionDecoder MouseDown))
                , case model.dragging of
                    Just _ ->
                        TypedSvg.Events.on "mousemove" (VirtualDom.Custom (positionDecoder MouseMove))

                    Nothing ->
                        TypedSvg.Attributes.class []
                , TypedSvg.Events.onMouseUp MouseUp
                ]
            |> Element.html
            |> Element.el [ width fill ]
        ]


positionDecoder :
    (Point2d Pixels () -> msg)
    ->
        Json.Decode.Decoder
            { message : msg
            , stopPropagation : Bool
            , preventDefault : Bool
            }
positionDecoder toMsg =
    Json.Decode.field "__svgCoordinates"
        (Json.Decode.map2
            (\x y ->
                { message = toMsg (Point2d.pixels x y)
                , stopPropagation = True
                , preventDefault = True
                }
            )
            (Json.Decode.field "x" Json.Decode.float)
            (Json.Decode.field "y" Json.Decode.float)
        )


svgWidth : number
svgWidth =
    800


svgHeight : number
svgHeight =
    600


organWidth : number
organWidth =
    240


organHeight : number
organHeight =
    160


viewOrgan : Point2d Pixels () -> Organ -> TypedSvg.Core.Svg msg
viewOrgan pos organ =
    let
        { x, y } =
            Point2d.toPixels pos

        iifLeft : Bool -> String -> Float -> TypedSvg.Core.Svg msg
        iifLeft condition label dy =
            TypedSvg.text_
                [ TypedSvg.Attributes.InPx.x 8
                , TypedSvg.Attributes.InPx.y (32 + 24 * dy)
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorStart
                , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineHanging
                , if condition then
                    TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black)

                  else
                    TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.gray)
                ]
                [ TypedSvg.Core.text ("--> " ++ label) ]

        iifRight : Bool -> String -> Float -> TypedSvg.Core.Svg msg
        iifRight condition label dy =
            TypedSvg.text_
                [ TypedSvg.Attributes.InPx.x (organWidth - 8)
                , TypedSvg.Attributes.InPx.y (32 + 24 * dy)
                , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
                , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineHanging
                , if condition then
                    TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black)

                  else
                    TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.gray)
                ]
                [ TypedSvg.Core.text (label ++ " -->") ]
    in
    TypedSvg.g
        [ TypedSvg.Attributes.transform [ TypedSvg.Types.Translate x y ] ]
        [ TypedSvg.rect
            [ TypedSvg.Attributes.InPx.width organWidth
            , TypedSvg.Attributes.InPx.height organHeight
            , TypedSvg.Attributes.stroke (TypedSvg.Types.Paint Color.black)
            , TypedSvg.Attributes.fill (TypedSvg.Types.Paint Color.white)
            ]
            []
        , TypedSvg.text_
            [ TypedSvg.Attributes.InPx.x (organWidth / 2)
            , TypedSvg.Attributes.InPx.y 8
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorMiddle
            , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineHanging
            ]
            [ TypedSvg.Core.text organ.name ]
        , TypedSvg.text_
            [ TypedSvg.Attributes.InPx.x 8
            , TypedSvg.Attributes.InPx.y 32
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorStart
            , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineHanging
            ]
            [ TypedSvg.Core.text ("Contour: " ++ String.fromInt organ.contour) ]
        , TypedSvg.text_
            [ TypedSvg.Attributes.InPx.x (organWidth - 8)
            , TypedSvg.Attributes.InPx.y 32
            , TypedSvg.Attributes.textAnchor TypedSvg.Types.AnchorEnd
            , TypedSvg.Attributes.dominantBaseline TypedSvg.Types.DominantBaselineHanging
            ]
            [ TypedSvg.Core.text ("Erogeny: " ++ String.fromInt organ.erogeny) ]
        , iifLeft organ.isSquishable "IS" 1
        , iifLeft organ.isGrippable "IG" 2
        , iifLeft organ.isPenetrable "IP" 3
        , iifLeft organ.isEnsheatheable "IE" 4
        , iifRight organ.canSquish "CS" 1
        , iifRight organ.canGrip "CG" 2
        , iifRight organ.canPenetrate "CP" 3
        , iifRight organ.canEnsheathe "CE" 4
        ]


viewOrgasm : PlayingModel -> Element PlayingMsg
viewOrgasm model =
    let
        modifiers : Int
        modifiers =
            if model.selectedTemperament == Just "Valiant" then
                if model.valiantModifier < model.meters.stamina then
                    model.valiantModifier

                else
                    0

            else
                0

        orgasmThreshold : Int
        orgasmThreshold =
            model.meters.sensitivity + model.meters.satiation + modifiers

        isOrgasm : Bool
        isOrgasm =
            model.meters.arousal > orgasmThreshold
    in
    Theme.column
        [ width fill
        ]
        [ if isOrgasm then
            paragraph
                [ Theme.padding
                , Border.width 1
                , Background.color Theme.purple
                , Font.color Theme.white
                ]
                (if model.selectedTemperament == Just "Valiant" then
                    [ text "You are having an orgasm!"
                    ]

                 else
                    [ text "You are having an orgasm! (You can try resisting "
                    , el [ Font.bold ] (text "Valiant")
                    , text "ly though)"
                    ]
                )

          else
            paragraph
                [ Theme.padding
                , Border.width 1
                ]
                (if model.selectedTemperament == Just "Valiant" && model.meters.arousal > model.meters.sensitivity + model.meters.satiation then
                    [ text "You are resisting "
                    , el [ Font.bold ] (text "Valiant")
                    , text "ly."
                    ]

                 else
                    [ text "You are not having an orgasm (yet!)." ]
                )
        , paragraph []
            [ text ("Arousal: " ++ String.fromInt model.meters.arousal)
            , el [ Font.bold ] <|
                if model.meters.arousal <= orgasmThreshold then
                    text " ≤ "

                else
                    text " > "
            , text
                ("Orgasm Threshold: "
                    ++ String.fromInt model.meters.sensitivity
                    ++ " (Sensitivity) + "
                    ++ String.fromInt model.meters.satiation
                    ++ " (Satiation) + "
                    ++ String.fromInt modifiers
                    ++ " (Modifiers) = "
                    ++ String.fromInt orgasmThreshold
                )
            ]
        , if model.selectedTemperament == Just "Valiant" then
            Theme.row [ width fill ]
                [ if model.valiantModifier < model.meters.stamina then
                    paragraph []
                        [ text "You are being "
                        , el [ Font.bold ] (text "Valiant")
                        , text " which currently gives you a +"
                        , el [ Font.bold ] (text (String.fromInt model.valiantModifier))
                        , text " modifier to your Orgasm Threshold"
                        ]

                  else
                    paragraph []
                        [ text "You are being "
                        , el [ Font.bold ] (text "Valiant")
                        , text " which would give you a +"
                        , el [ Font.bold ] (text (String.fromInt model.valiantModifier))
                        , text " modifier to your Orgasm Threshold, if you had enough "
                        , el [ Font.bold ] (text "Stamina")
                        ]
                , Theme.button [ width shrink, alignRight ]
                    { onPress = Just RollValiantModifier
                    , label = text "Re-Roll"
                    }
                ]

          else
            Element.none
        ]


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
            if move.cravingThreshold > model.meters.craving then
                Nothing

            else if selected then
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
                            ++ "] | "
                        )
                    , Theme.withHint "Craving Threshold" (text "CT")
                    , text ": "
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
        column : String -> (Int -> List Int -> String) -> Element.Column ( Int, List Int ) PlayingMsg
        column header toLabel =
            { width = shrink
            , header = el [ Font.center ] (text header)
            , view =
                \( cost, dice ) ->
                    Theme.selectableButton [ width fill ]
                        { onPress = Just (StimulationCost cost)
                        , label = text (toLabel cost dice)
                        , selected = cost == model.stimulationCost
                        }
            }

        columns : List (Element.Column ( Int, List Int ) PlayingMsg)
        columns =
            [ column "Stamina" <|
                \cost _ -> String.fromInt cost
            , column "Stimulation" <|
                \cost _ ->
                    if cost == 1 then
                        "0"

                    else
                        String.fromInt (cost * 2)
            , column "Dice Type" <|
                \_ dice ->
                    if List.isEmpty dice then
                        "No Roll"

                    else
                        dice
                            |> List.Extra.gatherEquals
                            |> List.map
                                (\( die, other ) ->
                                    (if List.isEmpty other then
                                        "1"

                                     else
                                        String.fromInt (1 + List.length other)
                                    )
                                        ++ "d"
                                        ++ String.fromInt die
                                )
                            |> String.join " and "
            ]
    in
    Theme.table []
        { data = stimulationDice
        , columns = columns
        }


stimulationDice : List ( Int, List Int )
stimulationDice =
    [ []
    , [ 4 ]
    , [ 6 ]
    , [ 8 ]
    , [ 10 ]
    , [ 12 ]
    , [ 10, 4 ]
    , [ 10, 6 ]
    , [ 10, 8 ]
    , [ 10, 10 ]
    , [ 12, 10 ]
    , [ 12, 12 ]
    , [ 10, 10, 6 ]
    , [ 10, 10, 8 ]
    , [ 10, 10, 10 ]
    , [ 12, 10, 10 ]
    , [ 12, 12, 10 ]
    , [ 12, 12, 12 ]
    ]
        |> List.indexedMap (\i d -> ( i + 1, d ))
