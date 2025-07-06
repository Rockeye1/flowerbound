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
import OrgansSurface exposing (OrganPosition)
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Data
import Persona.View
import Phosphor
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Route
import RouteBuilder exposing (StatefulRoute)
import Set exposing (Set)
import Shared
import Site
import Theme exposing (Element)
import Triple.Extra
import Types exposing (Action(..), Move, Persona, StimulationType(..))
import Ui.WithContext as Ui exposing (alignRight, alignTop, centerX, centerY, el, fill, height, px, row, shrink, text, width)
import Ui.WithContext.Font as Font
import Ui.WithContext.Input as Input
import Ui.WithContext.Layout as Layout
import Ui.WithContext.Prose exposing (paragraph)
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
    | RearrangeUnpaired
    | ShowAppendages Int String
    | HideOrganOrAppendage Int String
    | ShowHiddenOrgans


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
    | SelectOrgasm (Maybe Orgasm)
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
    = WaitingForPersona String
    | Playing PlayingModel


type alias PlayingModel =
    { player : PlayerModel
    , others : List PlayerModel
    , organsPositions : Dict OrganKey OrganPosition
    , dragging : Maybe ( OrganKey, Vector2d Pixels () )
    , loadPersonaText : String
    }


type alias PlayerModel =
    { notes : String
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe Temperament
    , selectedOrgasm : Maybe Orgasm
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


type Orgasm
    = Yes
    | No


type alias OrganKey =
    ( Int, String )


type alias Meters =
    { sensitivity : Int
    , arousal : Int
    , craving : Int
    , satiation : Int
    , stamina : Int
    , intensity : Int
    , stimulation : Int
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
                    ( WaitingForPersona url, Effect.none )

                Ok persona ->
                    ( Playing (initPlayingModel persona), Effect.none )

        LoadedFromFile (Ok persona) ->
            ( Playing (initPlayingModel persona), Effect.none )

        LoadedFromFile (Err _) ->
            -- let
            --     _ =
            --         Debug.log ("LoadedFromFile error\n" ++ e ++ "\n") ()
            -- in
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
                    ( case newModel of
                        Just new ->
                            new
                                |> checkOrgans
                                |> Playing

                        Nothing ->
                            WaitingForPersona ""
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
                (\( i, organ ) ( acc, pos, zIndex ) ->
                    case Dict.get ( i, organ ) acc of
                        Nothing ->
                            ( Dict.insert ( i, organ )
                                { position = Point2d.pixels (pos * 16 + 8) (pos * 32 + 8)
                                , zIndex = zIndex
                                , show = not (String.contains "-" organ)
                                }
                                acc
                            , pos + 1
                            , zIndex + 1
                            )

                        Just _ ->
                            ( acc, pos, zIndex )
                )
                ( filtered
                , 0
                , getNewZIndex filtered
                )
                expected
                |> Triple.Extra.first
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
                    ( Just { model | loadPersonaText = url }, Effect.none )

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
                        zIndex : Int
                        zIndex =
                            getNewZIndex model.organsPositions
                    in
                    ( { model
                        | organsPositions =
                            Dict.update key
                                (Maybe.map
                                    (\organ ->
                                        { organ
                                            | position =
                                                Point2d.translateBy delta position
                                                    |> clipOrganPosition
                                            , zIndex = zIndex
                                        }
                                    )
                                )
                                model.organsPositions
                      }
                        |> trySnap
                        |> Just
                    , Effect.none
                    )

        MouseUp ->
            ( Just { model | dragging = Nothing, organsPositions = reStack model.organsPositions }, Effect.none )

        RearrangeUnpaired ->
            ( Just { model | organsPositions = rearrangeUnpaired model.organsPositions }, Effect.none )

        Rearrange ->
            ( Just { model | organsPositions = rearrange model.organsPositions }, Effect.none )

        ShowAppendages i organName ->
            ( { model
                | organsPositions =
                    let
                        pos : Maybe (Point2d Pixels ())
                        pos =
                            Dict.get ( i, organName ) model.organsPositions
                                |> Maybe.map .position

                        total : Float
                        total =
                            Dict.foldl
                                (\key _ acc ->
                                    if isMatch key then
                                        acc + 1

                                    else
                                        acc
                                )
                                0
                                model.organsPositions

                        isMatch : OrganKey -> Bool
                        isMatch ( candidateI, candidateOrganName ) =
                            (i == candidateI)
                                && String.startsWith (organName ++ "-") candidateOrganName
                    in
                    Dict.foldl
                        (\key position (( dict, count ) as acc) ->
                            if isMatch key then
                                ( Dict.insert key
                                    { position
                                        | show = True
                                        , position =
                                            case pos of
                                                Nothing ->
                                                    position.position

                                                Just organPosition ->
                                                    organPosition
                                                        |> Point2d.translateBy
                                                            (Vector2d.pixels
                                                                ((count - (total - 1) / 2) * (OrgansSurface.organWidth + snapDistance))
                                                                (OrgansSurface.organHeight + snapDistance)
                                                            )
                                    }
                                    dict
                                , count + 1
                                )

                            else
                                acc
                        )
                        ( model.organsPositions, 0 )
                        model.organsPositions
                        |> Tuple.first
              }
                |> Just
            , Effect.none
            )

        HideOrganOrAppendage i organName ->
            ( { model
                | organsPositions =
                    Dict.update ( i, organName )
                        (Maybe.map (\position -> { position | show = False }))
                        model.organsPositions
              }
                |> Just
            , Effect.none
            )

        ShowHiddenOrgans ->
            ( { model
                | organsPositions =
                    Dict.map
                        (\( _, organName ) position -> { position | show = position.show || not (String.contains "-" organName) })
                        model.organsPositions
              }
                |> Just
            , Effect.none
            )


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

        SelectOrgasm selectedOrgasm ->
            ( { player | selectedOrgasm = selectedOrgasm } |> Just
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
                        | arousal = 0
                        , sensitivity = 0
                        , intensity = 0
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


rearrangeUnpaired : Dict OrganKey OrganPosition -> Dict OrganKey OrganPosition
rearrangeUnpaired organsPositions =
    let
        ( paired, unpaired ) =
            organsPositions
                |> Dict.toList
                |> List.partition (isPaired organsPositions)
    in
    unpaired
        |> Dict.Extra.groupBy
            (\( ( i, _ ), { show } ) ->
                ( if show then
                    0

                  else
                    1
                , i
                )
            )
        |> Dict.toList
        |> List.foldl
            (\( _, group ) ( fromY, acc ) ->
                ( fromY + OrgansSurface.organHeight + 32 * toFloat (List.length group - 1) + 8
                , (group
                    |> List.sortBy
                        (\( _, { position } ) ->
                            let
                                { x, y } =
                                    Point2d.toPixels position
                            in
                            x + 8 * y
                        )
                    |> List.indexedMap
                        (\j ( key, { show } ) ->
                            ( key
                            , { position =
                                    Point2d.pixels
                                        (16 * toFloat j + 8)
                                        (fromY + (32 * toFloat j))
                              , zIndex = 0
                              , show = show
                              }
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


rearrange : Dict OrganKey OrganPosition -> Dict OrganKey OrganPosition
rearrange organsPositions =
    organsPositions
        |> Dict.toList
        |> Dict.Extra.groupBy
            (\( ( i, _ ), { show } ) ->
                ( if show then
                    0

                  else
                    1
                , i
                )
            )
        |> Dict.toList
        |> List.foldl
            (\( _, group ) ( fromY, acc ) ->
                ( fromY + OrgansSurface.organHeight + 32 * toFloat (List.length group - 1) + 8
                , (group
                    |> List.sortBy
                        (\( _, { position } ) ->
                            let
                                { x, y } =
                                    Point2d.toPixels position
                            in
                            x + 8 * y
                        )
                    |> List.indexedMap
                        (\j ( key, { show } ) ->
                            ( key
                            , { position =
                                    Point2d.pixels
                                        (16 * toFloat j + 8)
                                        (fromY + (32 * toFloat j))
                              , zIndex = 0
                              , show = show
                              }
                            )
                        )
                  )
                    ++ acc
                )
            )
            ( 8, [] )
        |> Tuple.second
        |> Dict.fromList
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
            (\( _, { position } ) ->
                let
                    { x, y } =
                        Point2d.toPixels position
                in
                x + 8 * y
            )
        |> List.indexedMap (\i ( key, position ) -> ( key, { position | zIndex = i } ))
        |> Dict.fromList


trySnap : PlayingModel -> PlayingModel
trySnap model =
    let
        sorted : List ( OrganKey, OrganPosition )
        sorted =
            model.organsPositions
                |> Dict.toList
                |> List.sortBy (\( _, { zIndex } ) -> -zIndex)

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


snapDistance : number
snapDistance =
    4


trySnapTo :
    ( OrganKey, OrganPosition )
    -> ( OrganKey, OrganPosition )
    -> Maybe ( OrganKey, OrganPosition )
trySnapTo ( _, target ) ( key, { position, zIndex, show } ) =
    if not show || not target.show then
        Nothing

    else
        let
            leftSnap : Point2d Pixels ()
            leftSnap =
                target.position
                    |> Point2d.translateBy (Vector2d.pixels (-snapDistance - OrgansSurface.organWidth) 0)

            tryPos : Maybe (Point2d Pixels ())
            tryPos =
                case trySnapHorizontallyToPoint leftSnap position of
                    Just newPos ->
                        Just newPos

                    Nothing ->
                        let
                            rightSnap : Point2d Pixels ()
                            rightSnap =
                                target.position
                                    |> Point2d.translateBy (Vector2d.pixels (snapDistance + OrgansSurface.organWidth) 0)
                        in
                        trySnapHorizontallyToPoint rightSnap position
        in
        tryPos
            |> Maybe.map
                (\newPos ->
                    ( key
                    , { position = newPos
                      , zIndex = zIndex
                      , show = show
                      }
                    )
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
                            (Pixels.pixels (OrgansSurface.organHeight / 2 + snapDistance / 2))
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
                                (Pixels.pixels (-OrgansSurface.organHeight / 2 - snapDistance / 2))
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


getNewZIndex : Dict OrganKey OrganPosition -> Int
getNewZIndex organsPositions =
    Dict.foldl
        (\_ { zIndex } acc -> max (zIndex + 1) acc)
        0
        organsPositions


raycast : PlayingModel -> Point2d Pixels () -> Maybe ( OrganKey, Vector2d Pixels () )
raycast model position =
    model.organsPositions
        |> Dict.toList
        |> List.sortBy (\( _, { zIndex } ) -> -zIndex)
        |> List.Extra.findMap
            (\( key, organPosition ) ->
                let
                    vec : Vector2d Pixels ()
                    vec =
                        Vector2d.from position organPosition.position

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

        model : PlayingModel
        model =
            { player = player
            , others = []
            , organsPositions = Dict.empty
            , dragging = Nothing
            , loadPersonaText = ""
            }
                |> checkOrgans
    in
    { model | organsPositions = rearrange model.organsPositions }


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
        , stimulation = 0
        }
    , selectedMove = Nothing
    , selectedTemperament = Nothing
    , selectedOrgasm = Nothing
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
        case model of
            WaitingForPersona value ->
                Theme.column
                    [ Theme.padding
                    , centerX
                    , centerY
                    , Ui.border 1
                    , Theme.shadow
                    ]
                    [ Theme.row
                        [ Theme.fontColorAccent
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
                        , text = value
                        }
                    ]
                    |> Ui.el
                        [ height fill
                        , Theme.backgroundColorBackground
                        ]
                    |> Ui.map PagesMsg.fromMsg

            Playing playingModel ->
                (viewPersonas playingModel
                    :: viewPlaying shared playingModel
                )
                    |> Theme.column
                        [ Theme.padding
                        , height fill
                        , Theme.backgroundColorBackground
                        ]
                    |> Ui.map PlayingMsg
                    |> Ui.updateContext (\context -> { context | colors = Persona.toColors playingModel.player.persona })
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
                |> Ui.updateContext (\context -> { context | colors = Persona.toColors persona })
        )
        (playingModel.player :: playingModel.others)
        ++ [ Theme.column [ centerX, centerY ]
                [ Theme.row
                    [ Theme.fontColorAccent
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
                    , text = playingModel.loadPersonaText
                    }
                ]
           ]
        |> Theme.row [ Ui.wrap, centerX ]


loadPersona :
    { loadFromUrl : String -> msg
    , loadFromFile : msg
    , text : String
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
                , text = config.text
                , onChange = config.loadFromUrl
                , placeholder = Just "Paste the Persona URL here"
                }
            ]
        , el
            [ centerX
            , Theme.fontColorAccent
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
        , el [ centerX, Theme.fontColorAccent ]
            (text "or")
        , Theme.link [ centerX ]
            { label = text "Go to the editor"
            , route =
                Route.Persona__Name___Data__
                    { name = Persona.default.name
                    , data = Nothing
                    }
            }
        ]


viewPlaying : Shared.Model -> PlayingModel -> List (Element PlayingMsg)
viewPlaying shared model =
    [ viewOrgans shared model
    , List.map (Ui.map (PlayerMsg Nothing)) (viewTurn model.player)
    ]
        |> List.concat


viewTurn : PlayerModel -> List (Element PlayerMsg)
viewTurn player =
    [ viewMeters player
    , viewNotes player
    , viewOrgasmButtons player
    , viewOrgasm player
    , viewStimulationResolve player
    , viewTemperaments player
    , viewStatusChecks player
    , viewMoves player
    , viewStimulationTable player
    ]
        |> List.concat


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
            { icon = Icons.show
            , title = "Show all hidden organs"
            , onPress = Just ShowHiddenOrgans
            }
        , Theme.iconButton
            [ alignRight
            ]
            { icon = Icons.rearrange
            , title = "Rearrange unpaired organs"
            , onPress = Just RearrangeUnpaired
            }
        , Theme.iconButton
            [ alignRight
            ]
            { icon = Icons.reset
            , title = "Rearrange all organs"
            , onPress = Just Rearrange
            }
        ]
    , OrgansSurface.view
        { mouseDown = MouseDown
        , mouseUp = MouseUp
        , mouseMove = MouseMove
        , showAppendages = ShowAppendages
        , hideOrganOrAppendage = HideOrganOrAppendage
        }
        model
        |> Ui.html
        |> Theme.el
            [ width <| px (floor OrgansSurface.width)
            , height <| px OrgansSurface.height
            , Ui.border 1
            , Theme.backgroundColorBackground
            ]
        |> Theme.el
            [ centerX
            , (OrgansSurface.width + 8)
                |> floor
                |> min (shared.width - 2 * Theme.rhythm)
                |> px
                |> width
            , height <| px (OrgansSurface.height + 8)
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
    [ el [ Font.bold ] (text "Orgasm Check - At the START of your turn, check if you are Having an Orgasm")
    , if isOrgasm then
        paragraph
            [ Theme.padding
            , Ui.border 1
            , Theme.backgroundColorAccent
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


viewStimulationResolve : PlayerModel -> List (Element PlayerMsg)
viewStimulationResolve player =
    let
        meters : Meters
        meters =
            player.meters

        isOrgasm : Bool
        isOrgasm =
            case Maybe.withDefault No player.selectedOrgasm of
                Yes ->
                    True

                No ->
                    False
    in
    [ el [ Font.bold ] (text "Stimulation Helper - Quick Reference on how to resolve (Positive) Stimulation")
    , (statusMeter "Stimulation" meters.stimulation 30 <| \newValue -> { meters | stimulation = newValue })
        |> Layout.rowWithConstraints [ Layout.byContent, Layout.fill ] []
        |> Ui.map UpdateMeters
    , if isOrgasm then
        paragraph
            [ Theme.padding
            , Ui.border 1
            ]
            [ text
                ("You are Having an Orgasm, so for this amount of Stimulation: "
                    ++ String.fromInt meters.stimulation
                    ++ ", you should adjust the following meters: (Satiation +"
                    ++ String.fromInt meters.stimulation
                    ++ ") (Sensitivity +"
                    ++ String.fromInt (meters.stimulation // 2)
                    ++ ")."
                )
            ]

      else
        paragraph
            [ Theme.padding
            , Ui.border 1
            ]
            [ text
                ("You are not Having an Orgasm, so for this amount of Stimulation: "
                    ++ String.fromInt meters.stimulation
                    ++ ", you should adjust the following meters: (Arousal +"
                    ++ String.fromInt meters.stimulation
                    ++ "). Make sure to check for Overstimulation and Understimulation!"
                )
            ]
    , case player.ardorCheck of
        Just ardorCheckFlat ->
            let
                persona : Persona
                persona =
                    player.persona

                intensityAmount : Int
                intensityAmount =
                    if meters.stimulation <= persona.ardor then
                        1

                    else if meters.stimulation <= ardorCheckFlat then
                        3

                    else if meters.stimulation <= (10 + persona.ardor) then
                        5

                    else
                        7
            in
            paragraph
                [ Theme.padding
                , Ui.border 1
                ]
                [ text
                    ("Your Ardor is "
                        ++ String.fromInt persona.ardor
                        ++ " and the result of your Ardor check was "
                        ++ String.fromInt ardorCheckFlat
                        ++ ", compared to a Stimulation of "
                        ++ String.fromInt meters.stimulation
                        ++ ", so you should adjust (Intensity) by "
                        ++ String.fromInt intensityAmount
                        ++ ". Make sure to re-roll the Ardor check for each new source of Stimulation!"
                    )
                ]

        Nothing ->
            paragraph
                [ Theme.padding
                , Ui.border 1
                ]
                [ text "To calculate Intensity, roll an Ardor check with the Status Checks dice below." ]
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


viewOrgasmButtons : PlayerModel -> List (Element PlayerMsg)
viewOrgasmButtons model =
    [ el [ Font.bold, Ui.widthMin 300 ] (text "Am I Having an Orgasm? (Update at the START of your turn)")
    , [ ( Yes, "When receiving Stimulation: Add Stimulation to Satiation. Add 1/2 Stimulation rounded down to Sensitivity. Check for Overstimulation and increase Sensitivity if applicable. Do NOT apply Understimulation rules. Roll an Ardor check against the Stimulation and increase your Intensity according to the result.", "At the end of your turn, apply any Periodic effects. Then compare Satiation and Craving. If Satiation > Craving, -1 Craving, -1 Arousal, and +3 Sensitivity. If Craving > Satiation, -1 Satiation, +1 Arousal, +3 Sensitivity. If Craving = Satiation, +3 Sensitivity." )
      , ( No, "When receiving Stimulation: Add Stimulation to Arousal. Check for Understimulation and increase Craving if applicable (unless it's reciprocal Stimulation). Check for Overstimulation and increase Sensitivity if applicable. Roll an Ardor check against the Stimulation and increase your Intensity according to the result.", "At the end of your turn, apply any Periodic effects. Then compare Satiation and Craving. If Satiation > Craving, -1 Craving, -1 Arousal. If Craving > Satiation, -1 Satiation, +1 Arousal. If Craving = Satiation, do nothing." )
      ]
        |> List.map (viewOrgasmButton model)
        |> Theme.row [ Ui.wrap ]
    ]


viewOrgasmButton : PlayerModel -> ( Orgasm, String, String ) -> Element PlayerMsg
viewOrgasmButton model ( name, description, consequence ) =
    let
        selected : Bool
        selected =
            model.selectedOrgasm == Just name
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
                Just (SelectOrgasm Nothing)

            else
                Just (SelectOrgasm (Just name))
        , label =
            Theme.column [ alignTop ]
                (paragraph []
                    [ el [ Font.bold ] (text (orgasmToString name))
                    , text " "
                    , text description
                    ]
                    :: Theme.viewMarkdown consequence
                )
        }


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


orgasmToString : Orgasm -> String
orgasmToString orgasm =
    case orgasm of
        Yes ->
            "Having an Orgasm"

        No ->
            "Not Having an Orgasm"


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
                                        Theme.el
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
                                            , Theme.padding
                                            , Font.center
                                            , centerY
                                            ]
                                            child
                                    )
                                |> Theme.column
                                    [ Theme.backgroundColorBackground
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
                                        "2"

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
