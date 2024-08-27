module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Dict exposing (Dict)
import Dict.Extra
import Effect exposing (Effect)
import Element exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, paragraph, px, row, scrollbarX, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FatalError exposing (FatalError)
import File exposing (File)
import Head
import Head.Seo as Seo
import Icons
import List.Extra
import OrgansSurface
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Data
import Persona.View
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import RouteBuilder exposing (StatefulRoute)
import Set exposing (Set)
import Shared
import Site
import Theme
import Triple
import Types exposing (Attribute(..), Move, Persona, StimulationType(..))
import UrlPath exposing (UrlPath)
import Vector2d exposing (Vector2d)
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
    | UpdateFromFile
    | UpdateOther Int Persona
    | UpdateOtherFromFile Int
    | AddFromFile
    | AddFromUrl String
    | UpdateMeters Meters
    | SelectMove (Maybe String)
    | SelectTemperament (Maybe Temperament)
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
    | Rearrange
    | RemoveOther Int


type Model
    = WaitingForPersona
    | Playing PlayingModel


type alias PlayingModel =
    { persona : Persona
    , others : List Persona
    , organsPositions : Dict OrganKey OrganPosition
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe Temperament
    , valiantModifier : Int
    , stimulationRoll : Maybe (List ( Int, Int ))
    , dragging : Maybe ( OrganKey, Vector2d Pixels () )
    }


type Temperament
    = Innocent
    | Thoughtful
    | Perverse
    | Valiant


type alias OrganKey =
    ( Int, String )


type alias OrganPosition =
    ( Point2d Pixels (), Int )


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

        LoadedFromFile (Err _) ->
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
        expected : Set OrganKey
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

        filtered : Dict OrganKey OrganPosition
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
                            ( Dict.insert ( i, organ ) ( Point2d.pixels (pos * 16 + 8) (pos * 32 + 8), z ) acc
                            , pos + 1
                            , z + 1
                            )

                        Just _ ->
                            ( acc, pos, z )
                )
                ( filtered
                , 0
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

        RemoveOther index ->
            ( { model
                | others = List.Extra.removeAt index model.others
                , organsPositions =
                    model.organsPositions
                        |> Dict.toList
                        |> List.map
                            (\( ( i, name ), value ) ->
                                ( ( if i >= index then
                                        i - 1

                                    else
                                        i
                                  , name
                                  )
                                , value
                                )
                            )
                        |> Dict.fromList
              }
            , Effect.none
            )

        MouseDown position ->
            case raycast model position of
                Nothing ->
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
                        |> trySnap
                    , Effect.none
                    )

        MouseUp ->
            ( { model | dragging = Nothing, organsPositions = reStack model.organsPositions }, Effect.none )

        Rearrange ->
            ( { model | organsPositions = rearrange model.organsPositions }, Effect.none )


rearrange : Dict OrganKey OrganPosition -> Dict OrganKey OrganPosition
rearrange organsPositions =
    let
        ( paired, unpaired ) =
            organsPositions
                |> Dict.toList
                |> List.partition (isPaired organsPositions)
    in
    unpaired
        |> List.Extra.gatherEqualsBy
            (\( ( i, _ ), _ ) -> i)
        |> List.foldl
            (\( h, t ) ( fromY, acc ) ->
                let
                    group : List ( OrganKey, OrganPosition )
                    group =
                        h :: t
                in
                ( fromY + OrgansSurface.organHeight + 32 * toFloat (List.length t) + 8
                , (group
                    |> List.sortBy
                        (\( _, ( pos, _ ) ) ->
                            let
                                { x, y } =
                                    Point2d.toPixels pos
                            in
                            x + 8 * y
                        )
                    |> List.indexedMap
                        (\j ( key, _ ) ->
                            ( key
                            , ( Point2d.pixels
                                    (16 * toFloat j + 8)
                                    (fromY + (32 * toFloat j))
                              , 0
                              )
                            )
                        )
                  )
                    ++ acc
                )
            )
            ( 8, [] )
        |> Tuple.second
        |> Dict.fromList
        |> Dict.union (Dict.fromList paired)
        |> reStack


isPaired : Dict OrganKey OrganPosition -> ( OrganKey, OrganPosition ) -> Bool
isPaired organsPositions organ =
    Dict.Extra.find
        (\key option ->
            trySnapTo ( key, option ) organ /= Nothing
        )
        organsPositions
        /= Nothing


reStack :
    Dict OrganKey OrganPosition
    -> Dict OrganKey OrganPosition
reStack organsPositions =
    organsPositions
        |> Dict.toList
        |> List.sortBy
            (\( _, ( pos, _ ) ) ->
                let
                    { x, y } =
                        Point2d.toPixels pos
                in
                x + 8 * y
            )
        |> List.indexedMap (\i ( key, ( pos, _ ) ) -> ( key, ( pos, i ) ))
        |> Dict.fromList


trySnap : PlayingModel -> PlayingModel
trySnap model =
    let
        sorted : List ( OrganKey, OrganPosition )
        sorted =
            model.organsPositions
                |> Dict.toList
                |> List.sortBy (\( _, ( _, zOrder ) ) -> -zOrder)

        pair : List a -> List ( a, List a ) -> List ( a, List a )
        pair queue acc =
            case queue of
                [] ->
                    acc

                h :: tail ->
                    pair tail (( h, tail ) :: acc)

        toSnap : List ( OrganKey, OrganPosition )
        toSnap =
            pair sorted []
                |> List.filterMap
                    (\( organ, options ) ->
                        List.Extra.findMap
                            (\option ->
                                trySnapTo option organ
                            )
                            options
                    )
    in
    { model
        | organsPositions =
            List.foldl
                (\( key, value ) acc -> Dict.insert key value acc)
                model.organsPositions
                toSnap
    }


trySnapTo :
    ( OrganKey, OrganPosition )
    -> ( OrganKey, OrganPosition )
    -> Maybe ( OrganKey, OrganPosition )
trySnapTo ( _, ( targetPos, _ ) ) ( key, ( organPos, zOrder ) ) =
    let
        snapLimit : Quantity Float Pixels
        snapLimit =
            Pixels.pixels 16

        leftSnap : Point2d Pixels ()
        leftSnap =
            targetPos
                |> Point2d.translateBy (Vector2d.pixels (-4 - OrgansSurface.organWidth) 0)

        leftVector : Vector2d Pixels ()
        leftVector =
            Vector2d.from organPos leftSnap
    in
    if
        Vector2d.length leftVector
            |> Quantity.lessThan snapLimit
    then
        Just ( key, ( leftSnap, zOrder ) )

    else
        let
            rightSnap : Point2d Pixels ()
            rightSnap =
                targetPos
                    |> Point2d.translateBy (Vector2d.pixels (4 + OrgansSurface.organWidth) 0)

            rightVector : Vector2d Pixels ()
            rightVector =
                Vector2d.from organPos rightSnap
        in
        if
            Vector2d.length rightVector
                |> Quantity.lessThan snapLimit
        then
            Just ( key, ( rightSnap, zOrder ) )

        else
            Nothing


clipOrganPosition : Point2d Pixels () -> Point2d Pixels ()
clipOrganPosition position =
    let
        { x, y } =
            Point2d.toPixels position
    in
    Point2d.pixels
        (clamp 0 (OrgansSurface.width - OrgansSurface.organWidth) x)
        (clamp 0 (OrgansSurface.height - OrgansSurface.organHeight) y)


getNewZOrder : Dict OrganKey OrganPosition -> Int
getNewZOrder organsPositions =
    Dict.foldl
        (\_ ( _, z ) acc -> max (z + 1) acc)
        0
        organsPositions


raycast : PlayingModel -> Point2d Pixels () -> Maybe ( OrganKey, Vector2d Pixels () )
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
                if x <= 0 && x >= -OrgansSurface.organWidth && y <= 0 && y >= -OrgansSurface.organHeight then
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
    Seo.summary Site.defaultSummary |> Seo.website


data : BackendTask FatalError Data
data =
    BackendTask.succeed {}


view : RouteBuilder.App Data ActionData RouteParams -> Shared.Model -> Model -> View (PagesMsg Msg)
view _ shared model =
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
                        [ Icons.flower |> Icons.toElement
                        , text "Welcome to Flowerbound"
                        , Icons.flower |> Icons.toElement
                        ]
                    , loadPersona
                        { loadFromFile = LoadFromFile
                        , loadFromUrl = LoadFromUrl
                        }
                    ]

            Playing playingModel ->
                [ Theme.wrappedRow []
                    (viewPersonas playingModel)
                , viewPlaying shared playingModel
                ]
                    |> Theme.column [ Theme.padding ]
                    |> Element.map PlayingMsg
        )
            |> Element.map PagesMsg.fromMsg
    }


viewPersonas : PlayingModel -> List (Element PlayingMsg)
viewPersonas playingModel =
    Persona.View.persona
        [ alignTop ]
        { update = UpdatePersona
        , upload = UpdateFromFile
        , remove = Nothing
        , persona = playingModel.persona
        }
        :: List.indexedMap
            (\i other ->
                Persona.View.persona
                    [ alignTop ]
                    { update = UpdateOther i
                    , upload = UpdateOtherFromFile i
                    , remove = Just (RemoveOther i)
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
                    [ Icons.flower |> Icons.toElement
                    , text "Add another player"
                    , Icons.flower |> Icons.toElement
                    ]
                , loadPersona
                    { loadFromFile = AddFromFile
                    , loadFromUrl = AddFromUrl
                    }
                ]
           ]


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
            , Theme.iconButton
                [ alignRight
                ]
                { onPress = Just config.loadFromFile
                , icon = Icons.upload
                , title = "Upload"
                }
            ]
        ]


viewPlaying : Shared.Model -> PlayingModel -> Element PlayingMsg
viewPlaying shared ({ meters, persona } as model) =
    Theme.column [ width fill ]
        [ viewOrgans shared model
        , el [ Font.bold ] (text "Status meters")
        , Theme.row []
            [ Theme.iconAndTextButton [ width fill ]
                { onPress = Just BeginEncounter
                , label = "Begin Encounter"
                , icon = Icons.beginEncounter
                }
            , Theme.iconAndTextButton [ width fill ]
                { onPress = Just Rest
                , icon = Icons.rest
                , label = "Rest"
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
        , Theme.wrappedRow [ width <| Element.maximum (shared.width - 2 * Theme.rhythm) fill ]
            [ Theme.column
                [ alignTop
                , width fill
                ]
                [ el [ Font.bold, width <| Element.minimum 300 fill ] (text "Moves")
                , text "Choose a move."
                , viewMoves model
                ]
            , Theme.column
                [ alignTop
                , width fill
                ]
                [ Theme.column [ centerX ]
                    [ el [ Font.bold ] (text "Stimulation")
                    , text "Choose a stamina cost."
                    , Theme.row [ width fill ]
                        [ viewRoll model
                        , Theme.iconButton [ alignRight ]
                            { onPress =
                                if model.stimulationCost == 1 then
                                    Nothing

                                else
                                    Just RollStimulation
                            , icon = Icons.roll
                            , title =
                                case model.stimulationRoll of
                                    Nothing ->
                                        "Roll"

                                    Just _ ->
                                        "Reroll"
                            }
                        ]
                    , staminaTable model
                    ]
                ]
            , Theme.column
                [ alignTop
                , width fill
                ]
                [ el [ Font.bold, width <| Element.minimum 300 fill ] (text "Temperaments")
                , text "(Optionally) choose a Temperament"
                , viewTemperaments model
                ]
            ]
        ]


viewRoll : PlayingModel -> Element PlayingMsg
viewRoll model =
    case model.stimulationRoll of
        Nothing ->
            el [ Font.color Theme.gray ]
                (text "No roll yet.")

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

                firstColumn : Element.Column Bool msg
                firstColumn =
                    { width = shrink
                    , header = Element.none
                    , view =
                        \firstRow ->
                            let
                                label : String
                                label =
                                    if firstRow then
                                        "+"

                                    else
                                        "-"

                                topBorder : Int
                                topBorder =
                                    if firstRow then
                                        1

                                    else
                                        0
                            in
                            Theme.el
                                [ Background.color Theme.lightPurple
                                , Theme.padding
                                , Font.center
                                , Border.widthEach
                                    { top = topBorder
                                    , bottom = 1
                                    , left = 1
                                    , right = 0
                                    }
                                ]
                                (text label)
                    }

                otherColumns : List (Element.Column Bool msg)
                otherColumns =
                    List.map
                        (\( ardent, timid ) ->
                            { width = shrink
                            , header = Element.none
                            , view =
                                \firstRow ->
                                    let
                                        label : String
                                        label =
                                            if firstRow then
                                                String.fromInt ardent

                                            else
                                                String.fromInt timid

                                        topBorder : Int
                                        topBorder =
                                            if firstRow then
                                                1

                                            else
                                                0
                                    in
                                    Theme.el
                                        [ Background.color Theme.lightPurple
                                        , Theme.padding
                                        , Font.alignRight
                                        , Border.widthEach
                                            { top = topBorder
                                            , bottom = 1
                                            , left = 1
                                            , right = 0
                                            }
                                        ]
                                        (text label)
                            }
                        )
                        results
            in
            row [ width fill ]
                [ Theme.table
                    [ Element.spacing 0
                    , width shrink
                    ]
                    { data = [ True, False ]
                    , columns = firstColumn :: otherColumns
                    }
                , Theme.el
                    [ Background.color Theme.lightPurple
                    , Theme.padding
                    , Border.width 1
                    , height fill
                    ]
                    (el [ centerY ] (text (String.fromInt raw)))
                , if raw == corrected then
                    Element.none

                  else
                    Theme.column
                        [ Background.color Theme.lightPurple
                        , Theme.padding
                        , Border.widthEach
                            { top = 1
                            , bottom = 1
                            , left = 0
                            , right = 1
                            }
                        , height fill
                        , spacing 0
                        ]
                        [ text ("PRW " ++ String.fromInt model.persona.prowess)
                        , el [ centerX ] (text "⇒")
                        ]
                , if raw == corrected then
                    Element.none

                  else
                    Theme.el
                        [ Background.color Theme.lightPurple
                        , Theme.padding
                        , Border.widthEach
                            { top = 1
                            , bottom = 1
                            , left = 0
                            , right = 1
                            }
                        , height fill
                        ]
                        (el
                            [ centerY
                            ]
                            (text (String.fromInt corrected))
                        )
                , el [ width fill ] Element.none
                ]


viewOrgans : Shared.Model -> PlayingModel -> Element PlayingMsg
viewOrgans shared model =
    Theme.column [ width fill ]
        [ Theme.row [ width fill ]
            [ el [ Font.bold ] (text "Organs")
            , Theme.iconButton
                [ alignRight
                ]
                { icon = Icons.reset
                , title = "Rearrange unpaired organs"
                , onPress = Just Rearrange
                }
            ]
        , OrgansSurface.view
            { mouseDown = MouseDown
            , mouseUp = MouseUp
            , mouseMove = MouseMove
            }
            model
            |> Element.html
            |> Theme.el
                [ width <| px OrgansSurface.width
                , height <| px (ceiling OrgansSurface.height)
                , Border.width 1
                , Background.color Theme.lightPurple
                ]
            |> Theme.el
                [ centerX
                , (floor <| OrgansSurface.width + 8)
                    |> min (shared.width - 2 * Theme.rhythm)
                    |> px
                    |> width
                , height <| px <| floor (OrgansSurface.height + 8)
                , scrollbarX
                ]
        ]


viewOrgasm : PlayingModel -> Element PlayingMsg
viewOrgasm model =
    let
        modifiers : Int
        modifiers =
            if model.selectedTemperament == Just Valiant then
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
                (if model.selectedTemperament == Just Valiant then
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
                (if model.selectedTemperament == Just Valiant && model.meters.arousal > model.meters.sensitivity + model.meters.satiation then
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
        , if model.selectedTemperament == Just Valiant then
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
                , Theme.iconButton [ width shrink, alignRight ]
                    { onPress = Just RollValiantModifier
                    , icon = Icons.roll
                    , title = "Re-Roll"
                    }
                ]

          else
            Element.none
        ]


viewTemperaments : PlayingModel -> Element PlayingMsg
viewTemperaments model =
    [ ( Innocent, "You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Craving** value, drain the value of the result from your **Sensitivity**." )
    , ( Thoughtful, "You are dwelling on the emotions and emotional implications and the shape of your future.", "When calculating the Aftermath of your turn, first roll a **Moxie Check**. If the result is _less_ than your current **Arousal** value, drain the value of the result from your **Satiation**." )
    , ( Perverse, "You are excited on a conceptual, kinky level, captivated and compelled.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Sensitivity** value, add the result to your **Craving** value." )
    , ( Valiant, "You are proud of yourself for enduring, but you are enduring rather than enjoying.", "When calculating whether or not you are currently having an **Orgasm**, roll a **Moxie Check**. If the result is _less_ than your current **Stamina** value, add the result to your Orgasm Threshold as a Modifier." )
    ]
        |> List.map (viewTemperament model)
        |> Theme.wrappedRow [ width fill ]


viewTemperament : PlayingModel -> ( Temperament, String, String ) -> Element PlayingMsg
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
                    [ el [ Font.bold ] (text (temperamentToString name))
                    , text " "
                    , text description
                    ]
                    :: Theme.viewMarkdown consequence
                )
        }


temperamentToString : Temperament -> String
temperamentToString temperament =
    case temperament of
        Valiant ->
            "Valiant"

        Innocent ->
            "Innocent"

        Thoughtful ->
            "Thoughtful"

        Perverse ->
            "Perverse"


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
