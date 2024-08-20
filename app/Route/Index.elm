module Route.Index exposing (ActionData, Data, Model, Msg, RouteParams, route)

import BackendTask exposing (BackendTask)
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
import List.Extra
import MimeType
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Persona
import Persona.Codec
import Persona.Data
import Persona.View
import RouteBuilder exposing (StatefulRoute)
import Set exposing (Set)
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


type Model
    = WaitingForPersona
    | Playing PlayingModel


type alias PlayingModel =
    { persona : Persona
    , others : List Persona
    , organsPositions : Dict ( Int, String ) ( Int, Int )
    , stimulationCost : Int
    , meters : Meters
    , selectedMove : Maybe String
    , selectedTemperament : Maybe String
    , valiantModifier : Int
    , stimulationRoll : Maybe (List ( Int, Int ))
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
    in
    { model
        | organsPositions =
            Set.foldl
                (\( i, organ ) ( pos, acc ) ->
                    case Dict.get ( i, organ ) acc of
                        Nothing ->
                            ( pos + 10, Dict.insert ( i, organ ) ( pos, pos ) acc )

                        Just _ ->
                            ( pos, acc )
                )
                ( 10
                , Dict.filter
                    (\key _ -> Set.member key expected)
                    model.organsPositions
                )
                expected
                |> Tuple.second
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
        , Theme.wrappedRow [ width fill ]
            [ Theme.column
                [ width fill
                , alignTop
                ]
                [ el [ Font.bold ] (text "Moves")
                , text "Choose a move."
                , viewMoves model
                ]
            , Theme.column
                [ alignTop ]
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
            , Theme.column [ alignTop ]
                [ el [ Font.bold ] (text "Temperaments")
                , text "(Optionally) choose a Temperament"
                , viewTemperaments model
                ]
            ]
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
