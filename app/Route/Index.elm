module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
import Dict exposing (Dict)
import Dict.Extra
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import File exposing (File)
import Head
import Head.Seo as Seo
import Html.Attributes
import Icons
import List.Extra
import OrgansSurface
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Data
import Persona.View
import Phosphor
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import RouteBuilder exposing (StatefulRoute)
import Set exposing (Set)
import Shared
import Site
import Theme
import Triple
import Types exposing (Action(..), Move, Persona, StimulationType(..))
import Ui exposing (Element, alignRight, alignTop, centerX, centerY, el, fill, height, px, row, shrink, text, width)
import Ui.Font as Font
import Ui.Input as Input
import Ui.Layout as Layout
import Ui.Prose exposing (paragraph)
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
    , dragging : Maybe ( OrganKey, Vector2d Pixels () )
    , player : PlayerModel
    }


type alias PlayerModel =
    { stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe Temperament
    , valiantModifier : Int
    , stimulationRoll : Maybe (List ( Int, Int ))
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
    , intensity : Int
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
        alterPlayer : (PlayerModel -> PlayerModel) -> PlayingModel
        alterPlayer f =
            { model | player = f model.player }

        alterMeters : (Meters -> Meters) -> PlayingModel
        alterMeters f =
            alterPlayer (\player -> { player | meters = f player.meters })
    in
    case msg of
        StimulationCost stimulationCost ->
            ( alterPlayer
                (\player ->
                    { player
                        | stimulationCost = stimulationCost
                        , stimulationRoll = Nothing
                    }
                )
            , Effect.none
            )

        UpdateMeters newMeters ->
            ( alterMeters (\_ -> newMeters)
            , Effect.none
            )

        SelectMove selectedMove ->
            ( alterPlayer (\player -> { player | selectedMove = selectedMove })
            , Effect.none
            )

        SelectTemperament selectedTemperament ->
            ( alterPlayer (\player -> { player | selectedTemperament = selectedTemperament })
            , Effect.none
            )

        BeginEncounter ->
            ( alterMeters <| \meters -> { meters | stamina = 5 + model.persona.fitness }
            , Effect.none
            )

        Rest ->
            ( alterMeters
                (\meters ->
                    { meters
                        | arousal = 1
                        , sensitivity = 0
                    }
                )
            , Effect.batch
                [ Effect.rollCheck model.persona.ardor RestedSatiation
                , Effect.rollCheck model.persona.sanity RestedCraving
                ]
            )

        RestedSatiation satiation ->
            ( alterMeters (\meters -> { meters | satiation = min (Persona.maxSatiation model.persona) satiation })
            , Effect.none
            )

        RestedCraving craving ->
            ( alterMeters (\meters -> { meters | craving = min (Persona.maxCraving model.persona) craving })
            , Effect.none
            )

        RollValiantModifier ->
            ( model, Effect.rollCheck model.persona.moxie RolledValiantModifier )

        RolledValiantModifier modifier ->
            ( alterPlayer (\player -> { player | valiantModifier = modifier })
            , Effect.none
            )

        RollStimulation ->
            ( alterPlayer (\player -> { player | stimulationRoll = Nothing })
            , case
                stimulationDice
                    |> List.Extra.findMap
                        (\( cost, dice ) ->
                            if cost == model.player.stimulationCost then
                                Just dice

                            else
                                Nothing
                        )
                    |> Maybe.withDefault []
              of
                [] ->
                    Effect.none

                dice ->
                    Effect.RollStimulation dice RolledStimulation
            )

        RolledStimulation stimulationRoll ->
            ( alterPlayer (\player -> { player | stimulationRoll = Just stimulationRoll })
            , Effect.none
            )

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
                        let
                            ( finalOrgan, finalSnapped ) =
                                List.foldl
                                    (\option ( currentOrgan, hasSnapped ) ->
                                        case trySnapTo option organ of
                                            Nothing ->
                                                ( currentOrgan, hasSnapped )

                                            Just nextOrgan ->
                                                ( nextOrgan, True )
                                    )
                                    ( organ, False )
                                    options
                        in
                        if finalSnapped then
                            Just finalOrgan

                        else
                            Nothing
                    )
    in
    { model
        | organsPositions =
            List.foldl
                (\( key, value ) acc -> Dict.insert key value acc)
                model.organsPositions
                toSnap
    }


snapLimit : Quantity Float Pixels
snapLimit =
    Pixels.pixels 24


trySnapTo :
    ( OrganKey, OrganPosition )
    -> ( OrganKey, OrganPosition )
    -> Maybe ( OrganKey, OrganPosition )
trySnapTo ( _, ( targetPos, _ ) ) ( key, ( organPos, zOrder ) ) =
    let
        leftSnap : Point2d Pixels ()
        leftSnap =
            targetPos
                |> Point2d.translateBy (Vector2d.pixels (-4 - OrgansSurface.organWidth) 0)

        tryPos : Maybe (Point2d Pixels ())
        tryPos =
            case trySnapHorizontallyToPoint leftSnap organPos of
                Just newPos ->
                    Just newPos

                Nothing ->
                    let
                        rightSnap : Point2d Pixels ()
                        rightSnap =
                            targetPos
                                |> Point2d.translateBy (Vector2d.pixels (4 + OrgansSurface.organWidth) 0)
                    in
                    trySnapHorizontallyToPoint rightSnap organPos
    in
    tryPos
        |> Maybe.map
            (\newPos ->
                ( key, ( newPos, zOrder ) )
            )


trySnapHorizontallyToPoint : Point2d Pixels () -> Point2d Pixels () -> Maybe (Point2d Pixels ())
trySnapHorizontallyToPoint snapPoint organPos =
    if Point2d.equalWithin snapLimit snapPoint organPos then
        Just snapPoint

    else
        let
            lower : Point2d Pixels ()
            lower =
                snapPoint
                    |> Point2d.translateBy
                        (Vector2d.xy
                            Quantity.zero
                            (Pixels.pixels (OrgansSurface.organHeight / 2 + 2))
                        )
        in
        if Point2d.equalWithin snapLimit lower organPos then
            Just lower

        else
            let
                upper : Point2d Pixels ()
                upper =
                    snapPoint
                        |> Point2d.translateBy
                            (Vector2d.xy
                                Quantity.zero
                                (Pixels.pixels (-OrgansSurface.organHeight / 2 - 2))
                            )
            in
            if Point2d.equalWithin snapLimit upper organPos then
                Just upper

            else
                let
                    distanceVector : Vector2d Pixels ()
                    distanceVector =
                        Vector2d.from organPos snapPoint
                in
                if
                    (Vector2d.xComponent distanceVector
                        |> Quantity.abs
                        |> Quantity.lessThan snapLimit
                    )
                        && (Vector2d.yComponent distanceVector
                                |> Quantity.abs
                                |> Quantity.lessThan (Pixels.pixels OrgansSurface.organHeight)
                           )
                then
                    Just
                        (organPos
                            |> Point2d.translateBy
                                (Vector2d.xy
                                    (Vector2d.xComponent distanceVector)
                                    Quantity.zero
                                )
                        )

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
    let
        player : PlayerModel
        player =
            { stimulationCost = 1
            , meters =
                { sensitivity = 0
                , arousal = 0
                , craving = 0
                , satiation = 0
                , stamina = 0
                , intensity = 0
                }
            , selectedMove = Nothing
            , selectedTemperament = Nothing
            , valiantModifier = 0
            , stimulationRoll = Nothing
            }
    in
    { persona = persona
    , others = []
    , organsPositions = Dict.empty
    , dragging = Nothing
    , player = player
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
                [ Theme.row [ Ui.wrap, centerX ]
                    (viewPersonas playingModel)
                , viewPlaying shared playingModel
                ]
                    |> Theme.column [ Theme.padding ]
                    |> Ui.map PlayingMsg
        )
            |> Ui.map PagesMsg.fromMsg
    }


viewPersonas : PlayingModel -> List (Element PlayingMsg)
viewPersonas playingModel =
    Persona.View.persona
        [ alignTop, centerX ]
        { update = UpdatePersona
        , upload = UpdateFromFile
        , remove = Nothing
        , persona = playingModel.persona
        }
        :: List.indexedMap
            (\i other ->
                Persona.View.persona
                    [ alignTop, centerX ]
                    { update = UpdateOther i
                    , upload = UpdateOtherFromFile i
                    , remove = Just (RemoveOther i)
                    , persona = other
                    }
            )
            playingModel.others
        ++ [ Theme.column [ centerX, centerY ]
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
    let
        label : { element : Element msg, id : Input.Label }
        label =
            Input.label "url" [] (text "URL")
    in
    Theme.column [ centerX ]
        [ Theme.column
            [ Ui.border 1
            , Theme.padding
            ]
            [ label.element
            , Theme.input [ Ui.widthMin 240 ]
                { label = label.id
                , text = ""
                , onChange = config.loadFromUrl
                , placeholder = Just "Paste the Persona URL here"
                }
            ]
        , el
            [ centerX
            , Font.color Theme.purple
            ]
            (text "or")
        , Theme.row
            [ Ui.border 1
            , Theme.padding
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
viewPlaying shared model =
    Theme.column []
        [ viewOrgans shared model
        , el [ Font.bold ] (text "Status meters")
        , paragraph []
            [ text "Before an encounter you should probably "
            , Theme.iconAndTextButton [ width shrink ]
                { onPress = Just Rest
                , icon = Icons.rest
                , label = "Rest"
                }
            , text " to reset your "
            , el [ Font.bold ] (text "Satiation")
            , text " and "
            , el [ Font.bold ] (text "Craving")
            , text " and then "
            , Theme.iconAndTextButton []
                { onPress = Just BeginEncounter
                , label = "Begin the Encounter"
                , icon = Icons.beginEncounter
                }
            , el [] Ui.none
            ]
        , viewMeters model
        , viewTurn shared model
        ]


viewTurn : Shared.Model -> PlayingModel -> Element PlayingMsg
viewTurn shared model =
    Theme.column []
        [ el [ Font.bold ] (text "Orgasm")
        , viewOrgasm model.player
        , Theme.row
            [ Ui.wrap
            , Ui.widthMax (shared.width - 2 * Theme.rhythm)
            ]
            [ Theme.column [ alignTop, centerX ]
                [ el [ Font.bold, Ui.widthMin 300 ] (text "Moves")
                , text "Choose a move."
                , viewMoves model
                ]
            , Theme.column [ alignTop, centerX ]
                [ el [ Font.bold ] (text "Stimulation")
                , text "Choose a stamina cost."
                , Theme.row []
                    [ viewRoll model
                    , Theme.iconButton [ alignRight ]
                        { onPress =
                            if model.player.stimulationCost == 1 then
                                Nothing

                            else
                                Just RollStimulation
                        , icon = Icons.roll
                        , title =
                            case model.player.stimulationRoll of
                                Nothing ->
                                    "Roll"

                                Just _ ->
                                    "Reroll"
                        }
                    ]
                , staminaTable model.player
                ]
            , Theme.column [ alignTop, centerX ]
                [ el [ Font.bold, Ui.widthMin 300 ] (text "Temperaments")
                , text "(Optionally) choose a Temperament"
                , viewTemperaments model.player
                ]
            ]
        ]


viewMeters : PlayingModel -> Element PlayingMsg
viewMeters ({ persona } as model) =
    let
        { meters } =
            model.player
    in
    [ statusMeter "Stamina" meters.stamina (Persona.maxStamina persona) <| \newValue -> { meters | stamina = newValue }
    , statusMeter "Satiation" meters.satiation (Persona.maxSatiation persona) <| \newValue -> { meters | satiation = newValue }
    , statusMeter "Craving" meters.craving (Persona.maxCraving persona) <| \newValue -> { meters | craving = newValue }
    , statusMeter "Sensitivity" meters.sensitivity (Persona.maxSensitivity persona) <| \newValue -> { meters | sensitivity = newValue }
    , statusMeter "Arousal" meters.arousal (Persona.maxArousal persona) <| \newValue -> { meters | arousal = newValue }
    , statusMeter "Intensity" meters.intensity 30 <| \newValue -> { meters | intensity = newValue }
    ]
        |> List.concat
        |> Layout.rowWithConstraints [ Layout.byContent, Layout.fill ] []
        |> Ui.map UpdateMeters


viewRoll : PlayingModel -> Element PlayingMsg
viewRoll model =
    case model.player.stimulationRoll of
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

                otherColumns : List (List (Element msg))
                otherColumns =
                    List.map
                        (\( ardent, timid ) ->
                            [ text (String.fromInt ardent)
                            , text (String.fromInt timid)
                            ]
                        )
                        results
            in
            ([ text "+", text "-" ]
                :: otherColumns
                ++ [ [ text (String.fromInt raw) ]
                   , if raw == corrected then
                        []

                     else
                        [ text ("PRW " ++ String.fromInt model.persona.prowess)
                        , el [ centerX ] (text "⇒")
                        ]
                   , if raw == corrected then
                        []

                     else
                        [ text (String.fromInt corrected) ]
                   ]
            )
                |> List.indexedMap
                    (\c children ->
                        if List.isEmpty children then
                            Ui.none

                        else
                            children
                                |> List.indexedMap
                                    (\r child ->
                                        el
                                            [ Ui.borderWith
                                                { top =
                                                    if r == 0 then
                                                        0

                                                    else
                                                        1
                                                , left = 0
                                                , right = 0
                                                , bottom = 0
                                                }
                                            , Ui.borderColor Theme.purple
                                            , Theme.padding
                                            , Font.center
                                            , centerY
                                            ]
                                            child
                                    )
                                |> Theme.column
                                    [ Ui.background Theme.lightPurple
                                    , Ui.borderWith
                                        { left =
                                            if c == 0 then
                                                1

                                            else
                                                0
                                        , top = 1
                                        , bottom = 1
                                        , right = 1
                                        }
                                    , Ui.borderColor Theme.purple
                                    , height fill
                                    , width shrink
                                    ]
                    )
                |> row []


viewOrgans : Shared.Model -> PlayingModel -> Element PlayingMsg
viewOrgans shared model =
    Theme.column []
        [ Theme.row []
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
            |> Ui.html
            |> Theme.el
                [ width <| px OrgansSurface.width
                , height <| px (ceiling OrgansSurface.height)
                , Ui.border 1
                , Ui.background Theme.lightPurple
                ]
            |> Theme.el
                [ centerX
                , (floor <| OrgansSurface.width + 8)
                    |> min (shared.width - 2 * Theme.rhythm)
                    |> px
                    |> width
                , height <| px <| floor (OrgansSurface.height + 8)
                , Ui.scrollableX
                ]
        ]


viewOrgasm : PlayerModel -> Element PlayingMsg
viewOrgasm player =
    let
        meters : Meters
        meters =
            player.meters

        modifiers : Int
        modifiers =
            if player.selectedTemperament == Just Valiant then
                if player.valiantModifier < meters.stamina then
                    player.valiantModifier

                else
                    0

            else
                0

        orgasmThreshold : Int
        orgasmThreshold =
            meters.sensitivity + meters.satiation + modifiers

        isOrgasm : Bool
        isOrgasm =
            meters.arousal > orgasmThreshold
    in
    Theme.column []
        [ if isOrgasm then
            paragraph
                [ Theme.padding
                , Ui.border 1
                , Ui.background Theme.purple
                , Font.color Theme.white
                ]
                (if player.selectedTemperament == Just Valiant then
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
                , Ui.border 1
                ]
                (if player.selectedTemperament == Just Valiant && meters.arousal > meters.sensitivity + meters.satiation then
                    [ text "You are resisting "
                    , el [ Font.bold ] (text "Valiant")
                    , text "ly."
                    ]

                 else
                    [ text "You are not having an orgasm (yet!)." ]
                )
        , paragraph []
            [ text ("Arousal: " ++ String.fromInt meters.arousal)
            , el [ Font.bold ] <|
                if meters.arousal <= orgasmThreshold then
                    text " ≤ "

                else
                    text " > "
            , text
                ("Orgasm Threshold: "
                    ++ String.fromInt meters.sensitivity
                    ++ " (Sensitivity) + "
                    ++ String.fromInt meters.satiation
                    ++ " (Satiation) + "
                    ++ String.fromInt modifiers
                    ++ " (Modifiers) = "
                    ++ String.fromInt orgasmThreshold
                )
            ]
        , if player.selectedTemperament == Just Valiant then
            Theme.row []
                [ if player.valiantModifier < meters.stamina then
                    paragraph []
                        [ text "You are being "
                        , el [ Font.bold ] (text "Valiant")
                        , text " which currently gives you a +"
                        , el [ Font.bold ] (text (String.fromInt player.valiantModifier))
                        , text " modifier to your Orgasm Threshold"
                        ]

                  else
                    paragraph []
                        [ text "You are being "
                        , el [ Font.bold ] (text "Valiant")
                        , text " which would give you a +"
                        , el [ Font.bold ] (text (String.fromInt player.valiantModifier))
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
            Ui.none
        ]


viewTemperaments : PlayerModel -> Element PlayingMsg
viewTemperaments model =
    [ ( Innocent, "You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Craving** value, drain the value of the result from your **Sensitivity**." )
    , ( Thoughtful, "You are dwelling on the emotions and emotional implications and the shape of your future.", "When calculating the Aftermath of your turn, first roll a **Moxie Check**. If the result is _less_ than your current **Arousal** value, drain the value of the result from your **Satiation**." )
    , ( Perverse, "You are excited on a conceptual, kinky level, captivated and compelled.", "Upon declaration, roll a **Moxie Check**. If the result is _less_ than your current **Sensitivity** value, add the result to your **Craving** value." )
    , ( Valiant, "You are proud of yourself for enduring, but you are enduring rather than enjoying.", "When calculating whether or not you are currently having an **Orgasm**, roll a **Moxie Check**. If the result is _less_ than your current **Stamina** value, add the result to your Orgasm Threshold as a Modifier." )
    ]
        |> List.map (viewTemperament model)
        |> Theme.row [ Ui.wrap ]


viewTemperament : PlayerModel -> ( Temperament, String, String ) -> Element PlayingMsg
viewTemperament model ( name, description, consequence ) =
    let
        selected : Bool
        selected =
            model.selectedTemperament == Just name
    in
    Theme.selectableButton
        [ Ui.widthMin 400
        , height fill
        , width fill
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
        |> List.map (viewMove model.player)
        |> Theme.column [ width shrink ]


viewMove : PlayerModel -> Move -> Element PlayingMsg
viewMove model move =
    let
        selected : Bool
        selected =
            model.selectedMove == Just move.name
    in
    Theme.selectableButton [ Font.alignLeft, width fill ]
        { onPress =
            if move.cravingThreshold > model.meters.craving then
                Nothing

            else if selected then
                Just (SelectMove Nothing)

            else
                Just (SelectMove (Just move.name))
        , selected = selected
        , label =
            Theme.column []
                [ paragraph []
                    ([ el [ Font.bold ] (text move.name)
                     , text
                        (" ("
                            ++ Types.stimulationTypeToString move.stimulationType
                            ++ ") ["
                        )
                     ]
                        ++ (move.actionCompatibility
                                |> List.map
                                    (\action ->
                                        action
                                            |> Types.actionToCanIcon
                                            |> Phosphor.toHtml
                                                [ Html.Attributes.style "border-bottom" "1px dotted black"
                                                , Html.Attributes.style "margin-bottom" "-3px"
                                                ]
                                            |> Ui.html
                                            |> Theme.withHint (Types.actionToString action)
                                    )
                                |> List.intersperse (text "/")
                           )
                        ++ [ text "] | "
                           , Theme.withHint "Craving Threshold" (text "CT")
                           , text ": "
                           , el [ Font.bold ] (text (String.fromInt move.cravingThreshold))
                           , text " |"
                           ]
                    )
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
      , actionCompatibility = [ Squishes, Grips ]
      , cravingThreshold = 0
      , description = "A light touch with no other effects."
      }
    , { name = "Rub"
      , stimulationType = Grind
      , actionCompatibility = [ Squishes, Grips, Penetrates, Ensheathes ]
      , cravingThreshold = 0
      , description = "A massaging motion with no other effects."
      }
    , { name = "Stroke"
      , stimulationType = Thrust
      , actionCompatibility = [ Grips, Penetrates, Ensheathes ]
      , cravingThreshold = 0
      , description = "A back-and-forth movement with no other effects."
      }
    ]


statusMeter : String -> Int -> Int -> (Int -> msg) -> List (Element msg)
statusMeter label value cap setter =
    [ el [ centerY ] (text label)
    , Theme.slider []
        { min = 0
        , max = cap
        , value = value
        , onChange = setter
        , label = label
        }
    ]


staminaTable : PlayerModel -> Element PlayingMsg
staminaTable model =
    let
        header : String -> Element msg
        header label =
            el [ Theme.style "white-space" "pre" ] (text label)
    in
    stimulationDice
        |> List.map
            (\( cost, dice ) ->
                [ Theme.selectableButton []
                    { onPress = Just (StimulationCost cost)
                    , label = text (String.fromInt cost)
                    , selected = cost == model.stimulationCost
                    }
                , Theme.selectableButton []
                    { onPress = Just (StimulationCost cost)
                    , label =
                        text
                            (if cost == 1 then
                                "0"

                             else
                                String.fromInt (cost * 2)
                            )
                    , selected = cost == model.stimulationCost
                    }
                , Theme.selectableButton []
                    { onPress = Just (StimulationCost cost)
                    , label =
                        text
                            (if List.isEmpty dice then
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
                            )
                    , selected = cost == model.stimulationCost
                    }
                ]
            )
        |> (::) [ header "Stamina", header "Stimulation", header "Dice Type" ]
        |> List.concat
        |> Layout.rowWithConstraints (List.repeat 3 Layout.byContent) [ Theme.spacing ]


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
