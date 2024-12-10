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
    = PlayerMsg (Maybe Int) PlayerMsg
    | AddFromFile
    | AddFromUrl String
    | AddFromFilePicked File
    | AddFromFileRead (Result String Persona)
    | MouseDown (Point2d Pixels ())
    | MouseMove (Point2d Pixels ())
    | MouseUp
    | Rearrange


type PlayerMsg
    = BeginEncounter
    | Rest
    | RestedSatiation Int
    | RestedCraving Int
    | RollValiantModifier
    | RolledValiantModifier Int
    | RollFitnessCheck
    | RolledFitnessCheck Int
    | DeleteFitnessCheck
    | RollGraceCheck
    | RolledGraceCheck Int
    | DeleteGraceCheck
    | RollArdorCheck
    | RolledArdorCheck Int
    | DeleteArdorCheck
    | RollSanityCheck
    | RolledSanityCheck Int
    | DeleteSanityCheck
    | RollProwessCheck
    | RolledProwessCheck Int
    | DeleteProwessCheck
    | RollMoxieCheck
    | RolledMoxieCheck Int
    | DeleteMoxieCheck
    | UpdateMeters Meters
    | StimulationCost Int
    | SelectMove (Maybe String)
    | SelectTemperament (Maybe Temperament)
    | RollStimulation
    | RolledStimulation (List ( Int, Int ))
    | DeleteStimulation
    | UpdatePersona Persona
    | UpdatePersonaFromFile
    | UpdatePersonaPicked File
    | UpdatePersonaRead (Result String Persona)
    | Remove
    | Notes String


type Model
    = WaitingForPersona
    | Playing PlayingModel


type alias PlayingModel =
    { player : PlayerModel
    , others : List PlayerModel
    , organsPositions : Dict OrganKey OrganPosition
    , dragging : Maybe ( OrganKey, Vector2d Pixels () )
    }


type alias PlayerModel =
    { notes : String
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe Temperament
    , valiantModifier : Int
    , fitnessCheck : Maybe Int
    , graceCheck : Maybe Int
    , ardorCheck : Maybe Int
    , sanityCheck : Maybe Int
    , prowessCheck : Maybe Int
    , moxieCheck : Maybe Int
    , stimulationRoll : Maybe (List ( Int, Int ))
    , persona : Persona
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
                    ( case newModel of
                        Just new ->
                            new
                                |> checkOrgans
                                |> Playing

                        Nothing ->
                            WaitingForPersona
                    , Effect.map PlayingMsg effect
                    )

        PickedFile file ->
            ( model, Effect.ReadPersonaFromMarkdown file LoadedFromFile )


checkOrgans : PlayingModel -> PlayingModel
checkOrgans model =
    let
        expected : Set OrganKey
        expected =
            (model.player :: model.others)
                |> List.indexedMap
                    (\index { persona } ->
                        persona.gendertrope
                            |> Persona.Data.gendertropeToRecord
                            |> .organs
                            |> List.concatMap
                                (\organ ->
                                    ( index - 1, organ.name )
                                        :: List.map
                                            (\appendage ->
                                                ( index - 1, organ.name ++ "-" ++ appendage.name )
                                            )
                                            organ.appendages
                                )
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


innerUpdate : PlayingMsg -> PlayingModel -> ( Maybe PlayingModel, Effect PlayingMsg )
innerUpdate msg model =
    case msg of
        PlayerMsg Nothing f ->
            let
                ( maybePlayer, cmd ) =
                    playerUpdate f model.player
            in
            case maybePlayer of
                Just newPlayer ->
                    ( { model | player = newPlayer }
                        |> checkOrgans
                        |> Just
                    , Effect.map (PlayerMsg Nothing) cmd
                    )

                Nothing ->
                    case model.others of
                        [] ->
                            ( Nothing, Effect.none )

                        first :: tail ->
                            ( { model | player = first, others = tail }
                                |> Just
                            , Effect.none
                            )

        PlayerMsg (Just index) f ->
            case List.Extra.getAt index model.others of
                Nothing ->
                    ( Just model, Effect.none )

                Just player ->
                    let
                        ( maybePlayer, cmd ) =
                            playerUpdate f player
                    in
                    case maybePlayer of
                        Just newPlayer ->
                            ( { model | others = List.Extra.setAt index newPlayer model.others }
                                |> checkOrgans
                                |> Just
                            , Effect.map (PlayerMsg (Just index)) cmd
                            )

                        Nothing ->
                            ( { model | others = List.Extra.removeAt index model.others }
                                |> Just
                            , Effect.none
                            )

        AddFromFile ->
            ( Just model, Effect.PickMarkdown AddFromFilePicked )

        AddFromFilePicked file ->
            ( Just model, Effect.ReadPersonaFromMarkdown file AddFromFileRead )

        AddFromFileRead (Ok persona) ->
            addPlayer model persona

        AddFromFileRead (Err _) ->
            -- TODO
            ( Just model, Effect.none )

        AddFromUrl url ->
            case Persona.Codec.fromUrl url of
                Err _ ->
                    -- TODO
                    ( Just model, Effect.none )

                Ok persona ->
                    addPlayer model persona

        MouseDown position ->
            case raycast model position of
                Nothing ->
                    ( Just model, Effect.none )

                Just ( key, delta ) ->
                    ( Just { model | dragging = Just ( key, delta ) }, Effect.none )

        MouseMove position ->
            case model.dragging of
                Nothing ->
                    ( Just model, Effect.none )

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
                        |> Just
                    , Effect.none
                    )

        MouseUp ->
            ( Just { model | dragging = Nothing, organsPositions = reStack model.organsPositions }, Effect.none )

        Rearrange ->
            ( Just { model | organsPositions = rearrange model.organsPositions }, Effect.none )


playerUpdate : PlayerMsg -> PlayerModel -> ( Maybe PlayerModel, Effect PlayerMsg )
playerUpdate msg ({ persona } as player) =
    let
        alterMeters : (Meters -> Meters) -> Maybe PlayerModel
        alterMeters f =
            Just { player | meters = f player.meters }
    in
    case msg of
        StimulationCost stimulationCost ->
            ( { player
                | stimulationCost = stimulationCost
                , stimulationRoll = Nothing
              }
                |> Just
            , Effect.none
            )

        UpdateMeters newMeters ->
            ( alterMeters (\_ -> newMeters)
            , Effect.none
            )

        SelectMove selectedMove ->
            ( { player | selectedMove = selectedMove } |> Just
            , Effect.none
            )

        SelectTemperament selectedTemperament ->
            ( { player | selectedTemperament = selectedTemperament } |> Just
            , Effect.none
            )

        BeginEncounter ->
            ( alterMeters <| \meters -> { meters | stamina = 5 + persona.fitness }
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
                [ Effect.rollCheck persona.ardor RestedSatiation
                , Effect.rollCheck persona.sanity RestedCraving
                ]
            )

        RestedSatiation satiation ->
            ( alterMeters (\meters -> { meters | satiation = min (Persona.maxSatiation persona) satiation })
            , Effect.none
            )

        RestedCraving craving ->
            ( alterMeters (\meters -> { meters | craving = min (Persona.maxCraving persona) craving })
            , Effect.none
            )

        RollValiantModifier ->
            ( Just player, Effect.rollCheck persona.moxie RolledValiantModifier )

        RolledValiantModifier modifier ->
            ( { player | valiantModifier = modifier } |> Just
            , Effect.none
            )

        RollFitnessCheck ->
            ( Just player, Effect.rollCheck persona.fitness RolledFitnessCheck )

        RolledFitnessCheck modifier ->
            ( { player | fitnessCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteFitnessCheck ->
            ( { player | fitnessCheck = Nothing } |> Just
            , Effect.none
            )

        RollGraceCheck ->
            ( Just player, Effect.rollCheck persona.grace RolledGraceCheck )

        RolledGraceCheck modifier ->
            ( { player | graceCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteGraceCheck ->
            ( { player | graceCheck = Nothing } |> Just
            , Effect.none
            )

        RollArdorCheck ->
            ( Just player, Effect.rollCheck persona.ardor RolledArdorCheck )

        RolledArdorCheck modifier ->
            ( { player | ardorCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteArdorCheck ->
            ( { player | ardorCheck = Nothing } |> Just
            , Effect.none
            )

        RollSanityCheck ->
            ( Just player, Effect.rollCheck persona.sanity RolledSanityCheck )

        RolledSanityCheck modifier ->
            ( { player | sanityCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteSanityCheck ->
            ( { player | sanityCheck = Nothing } |> Just
            , Effect.none
            )

        RollProwessCheck ->
            ( Just player, Effect.rollCheck persona.prowess RolledProwessCheck )

        RolledProwessCheck modifier ->
            ( { player | prowessCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteProwessCheck ->
            ( { player | prowessCheck = Nothing } |> Just
            , Effect.none
            )

        RollMoxieCheck ->
            ( Just player, Effect.rollCheck persona.moxie RolledMoxieCheck )

        RolledMoxieCheck modifier ->
            ( { player | moxieCheck = Just modifier } |> Just
            , Effect.none
            )

        DeleteMoxieCheck ->
            ( { player | moxieCheck = Nothing } |> Just
            , Effect.none
            )

        RollStimulation ->
            ( { player | stimulationRoll = Nothing } |> Just
            , case
                stimulationDice
                    |> List.Extra.findMap
                        (\( cost, dice ) ->
                            if cost == player.stimulationCost then
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
            ( { player | stimulationRoll = Just stimulationRoll } |> Just
            , Effect.none
            )

        DeleteStimulation ->
            ( { player | stimulationRoll = Nothing } |> Just
            , Effect.none
            )

        UpdatePersona newPersona ->
            ( { player | persona = newPersona } |> Just, Effect.none )

        UpdatePersonaFromFile ->
            ( Just player, Effect.PickMarkdown UpdatePersonaPicked )

        UpdatePersonaPicked file ->
            ( Just player, Effect.ReadPersonaFromMarkdown file UpdatePersonaRead )

        UpdatePersonaRead (Ok newPersona) ->
            ( { player | persona = newPersona } |> Just, Effect.none )

        UpdatePersonaRead (Err _) ->
            -- TODO
            ( Just player, Effect.none )

        Remove ->
            ( Nothing, Effect.none )

        Notes notes ->
            ( Just { player | notes = notes }, Effect.none )


addPlayer : PlayingModel -> Persona -> ( Maybe PlayingModel, Effect PlayingMsg )
addPlayer model persona =
    let
        added : PlayingModel
        added =
            { model | others = model.others ++ [ initPlayerModel persona ] }
                |> checkOrgans
    in
    ( Just { added | organsPositions = rearrange added.organsPositions }
    , Effect.none
    )


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
            initPlayerModel persona
    in
    { player = player
    , others = []
    , organsPositions = Dict.empty
    , dragging = Nothing
    }
        |> checkOrgans


initPlayerModel : Persona -> PlayerModel
initPlayerModel persona =
    { notes = ""
    , stimulationCost = 1
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
    , fitnessCheck = Nothing
    , graceCheck = Nothing
    , ardorCheck = Nothing
    , sanityCheck = Nothing
    , prowessCheck = Nothing
    , moxieCheck = Nothing
    , stimulationRoll = Nothing
    , persona = persona
    }


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
                (viewPersonas playingModel
                    :: viewPlaying shared playingModel
                )
                    |> Theme.column [ Theme.padding ]
                    |> Ui.map PlayingMsg
        )
            |> Ui.map PagesMsg.fromMsg
    }


viewPersonas : PlayingModel -> Element PlayingMsg
viewPersonas playingModel =
    List.indexedMap
        (\i { persona } ->
            let
                maybeIndex : Maybe Int
                maybeIndex =
                    if i == 0 then
                        Nothing

                    else
                        Just (i - 1)
            in
            Persona.View.persona
                [ alignTop, centerX ]
                { update = UpdatePersona
                , upload = UpdatePersonaFromFile
                , remove = Just Remove
                , persona = persona
                }
                |> Ui.map (PlayerMsg maybeIndex)
        )
        (playingModel.player :: playingModel.others)
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
        |> Theme.row [ Ui.wrap, centerX ]


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


viewPlaying : Shared.Model -> PlayingModel -> List (Element PlayingMsg)
viewPlaying shared model =
    [ viewOrgans shared model
    , List.map (Ui.map (PlayerMsg Nothing)) (viewMeters model.player)
    , List.map (Ui.map (PlayerMsg Nothing)) (viewTurn shared model.player)
    ]
        |> List.concat


restParagraph : Element PlayerMsg
restParagraph =
    paragraph []
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
        ]


viewMeters : PlayerModel -> List (Element PlayerMsg)
viewMeters { persona, meters } =
    [ el [ Font.bold ] (text "Status meters")
    , restParagraph
    , [ statusMeter "Stamina" meters.stamina (Persona.maxStamina persona) <| \newValue -> { meters | stamina = newValue }
      , statusMeter "Satiation" meters.satiation (Persona.maxSatiation persona) <| \newValue -> { meters | satiation = newValue }
      , statusMeter "Craving" meters.craving (Persona.maxCraving persona) <| \newValue -> { meters | craving = newValue }
      , statusMeter "Sensitivity" meters.sensitivity (Persona.maxSensitivity persona) <| \newValue -> { meters | sensitivity = newValue }
      , statusMeter "Arousal" meters.arousal (Persona.maxArousal persona) <| \newValue -> { meters | arousal = newValue }
      , statusMeter "Intensity" meters.intensity 30 <| \newValue -> { meters | intensity = newValue }
      ]
        |> List.concat
        |> Layout.rowWithConstraints [ Layout.byContent, Layout.fill ] []
        |> Ui.map UpdateMeters
    ]


viewTurn : Shared.Model -> PlayerModel -> List (Element PlayerMsg)
viewTurn shared player =
    [ viewNotes player
    , viewOrgasm player
    , viewTemperaments player
    , viewStatusChecks player
    , viewMoves player
    , viewStimulationTable player
    ]
        |> List.concat


viewNotes : PlayerModel -> List (Element PlayerMsg)
viewNotes player =
    let
        { element, id } =
            Input.label "notes-id" [ Font.bold ] (text "Notes")
    in
    [ element
    , Theme.multiline []
        { label = id
        , onChange = Notes
        , placeholder = Just "Write your personal notes here"
        , text = player.notes
        , spellcheck = True
        }
    ]


viewOrgans : Shared.Model -> PlayingModel -> List (Element PlayingMsg)
viewOrgans shared model =
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


viewOrgasm : PlayerModel -> List (Element PlayerMsg)
viewOrgasm player =
    let
        meters : Meters
        meters =
            player.meters

        modifiers : Int
        modifiers =
            if player.selectedTemperament == Just Valiant then
                if player.valiantModifier > meters.stamina then
                    meters.stamina

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
    [ el [ Font.bold ] (text "Orgasm")
    , if isOrgasm then
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
            [ if player.valiantModifier > meters.stamina then
                paragraph []
                    [ text "You are being "
                    , el [ Font.bold ] (text "Valiant")
                    , text " which currently gives you a +"
                    , el [ Font.bold ] (text (String.fromInt meters.stamina))
                    , text " modifier to your Orgasm Threshold."
                    ]

              else
                paragraph []
                    [ text "You are trying being "
                    , el [ Font.bold ] (text "Valiant")
                    , text " which would give you a +"
                    , el [ Font.bold ] (text (String.fromInt meters.stamina))
                    , text " modifier to your Orgasm Threshold, but you only rolled "
                    , el [ Font.bold ] (text (String.fromInt player.valiantModifier))
                    , text "."
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


viewStatusChecks : PlayerModel -> List (Element PlayerMsg)
viewStatusChecks player =
    let
        viewButtonAndResult : msg -> msg -> Phosphor.IconVariant -> String -> Maybe Int -> List (Element msg)
        viewButtonAndResult rollMsg deleteMsg icon label result =
            [ Theme.iconAndTextButton []
                { icon = icon
                , onPress = Just rollMsg
                , label = label
                }
            , Theme.row [] (viewResult deleteMsg result)
            ]

        viewResult : msg -> Maybe Int -> List (Element msg)
        viewResult deleteMsg result =
            case result of
                Nothing ->
                    []

                Just value ->
                    [ el [ Font.bold, centerY, alignRight ] (text (String.fromInt value))
                    , Theme.iconButton []
                        { icon = Icons.delete
                        , onPress = Just deleteMsg
                        , title = "Delete"
                        }
                    ]
    in
    [ el [ Font.bold ] (text "Status checks")
    , Layout.rowWithConstraints
        [ Layout.byContent
        , Layout.byContent
        , Layout.byContent
        , Layout.byContent
        , Layout.byContent
        , Layout.byContent
        ]
        [ Theme.spacing ]
        ([ viewButtonAndResult RollFitnessCheck DeleteFitnessCheck Icons.roll "Fitness" player.fitnessCheck
         , viewButtonAndResult RollGraceCheck DeleteGraceCheck Icons.roll "Grace" player.graceCheck
         , viewButtonAndResult RollArdorCheck DeleteArdorCheck Icons.roll "Ardor" player.ardorCheck
         , viewButtonAndResult RollSanityCheck DeleteSanityCheck Icons.roll "Sanity" player.sanityCheck
         , viewButtonAndResult RollProwessCheck DeleteProwessCheck Icons.roll "Prowess" player.prowessCheck
         , viewButtonAndResult RollMoxieCheck DeleteMoxieCheck Icons.roll "Moxie" player.moxieCheck
         ]
            |> List.Extra.transpose
            |> List.concat
        )
    ]


viewTemperaments : PlayerModel -> List (Element PlayerMsg)
viewTemperaments model =
    [ el [ Font.bold, Ui.widthMin 300 ] (text "Temperaments (optional)")
    , [ ( Innocent, "You are living in the moment and not worrying about the past or future. You feel safe, happy, and unquestioning.", "Upon declaration, roll a **Moxie Check**. While the result remains greater than your **Craving** value, you may transfer points from your **Sensitivity** to your **Craving**." )
      , ( Thoughtful, "You are dwelling on the emotions and emotional implications and the shape of your future.", "Upon declaration, roll a **Moxie Check**. While the result remains greater than your **Craving** value, you may transfer points from your **Satiation** to your **Craving**." )
      , ( Perverse, "You are excited on a conceptual, kinky level, captivated and compelled.", "Upon declaration, roll a **Moxie Check**. While the result remains greater than your **Arousal** value, you may transfer points from your **Craving** to your **Arousal**." )
      , ( Valiant, "You are proud of yourself for enduring, but you are enduring rather than enjoying.", "Upon declaration, roll a **Moxie Check**. While the result is greater than your **Stamina** value, add your **Stamina** value to your **Orgasm Threshold** as a Modifier." )
      ]
        |> List.map (viewTemperament model)
        |> Theme.row [ Ui.wrap ]
    ]


viewTemperament : PlayerModel -> ( Temperament, String, String ) -> Element PlayerMsg
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


viewMoves : PlayerModel -> List (Element PlayerMsg)
viewMoves player =
    [ el [ Font.bold, Ui.widthMin 300 ] (text "Moves")
    , Theme.row [ Ui.wrap ]
        (List.map
            (viewMove player)
            (defaultMoves ++ featureMoves player.persona)
        )
    ]


viewMove : PlayerModel -> Move -> Element PlayerMsg
viewMove model move =
    let
        selected : Bool
        selected =
            model.selectedMove == Just move.name
    in
    Theme.selectableButton
        [ Font.alignLeft
        , width fill
        , height fill
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


featureMoves : Persona -> List Move
featureMoves _ =
    -- TODO: implement this
    []


viewStimulationTable : PlayerModel -> List (Element PlayerMsg)
viewStimulationTable player =
    [ el [ Font.bold ] (text "Stimulation")
    , staminaTable player
    , Theme.row []
        [ Theme.iconAndTextButton [ alignRight ]
            { onPress =
                if player.stimulationCost == 1 then
                    Nothing

                else
                    Just RollStimulation
            , icon = Icons.roll
            , label =
                case player.stimulationRoll of
                    Nothing ->
                        "Roll"

                    Just _ ->
                        "Reroll"
            }
        , viewRoll player
        , Theme.column []
            [ case player.stimulationRoll of
                Nothing ->
                    Ui.none

                Just _ ->
                    Theme.iconButton [ alignRight ]
                        { onPress = Just DeleteStimulation
                        , icon = Icons.delete
                        , title = "Delete"
                        }
            ]
        ]
    ]


viewRoll : PlayerModel -> Element PlayerMsg
viewRoll player =
    case player.stimulationRoll of
        Nothing ->
            Ui.none

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
                        min 0 (raw + player.persona.prowess)

                    else if raw == 0 then
                        0

                    else
                        max 0 (raw - player.persona.prowess)

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
                        [ text ("PRW " ++ String.fromInt player.persona.prowess)
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


staminaTable : PlayerModel -> Element PlayerMsg
staminaTable model =
    let
        header : String -> Element msg
        header label =
            el [ Theme.style "white-space" "pre" ] (text label)

        columns : List (List (Element PlayerMsg))
        columns =
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
                                            |> String.join ", "
                                    )
                            , selected = cost == model.stimulationCost
                            }
                        ]
                    )
                |> (::) [ header "Stamina", header "Stimulation", header "Dice Type" ]
    in
    columns
        |> List.Extra.transpose
        |> List.concat
        |> Layout.rowWithConstraints
            (List.repeat (List.length columns) Layout.byContent)
            [ Theme.spacing ]


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
